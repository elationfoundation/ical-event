;;; notmuch-calendar.el --- inline iCalendar display from notmuch mail

;; Copyright (C) 2013  Jan Tatarik
;; Copyright (C) 2015  seamus tuohy

;; Author: Jan Tatarik <Jan.Tatarik@gmail.com>
;; Author: seamus tuohy <s2e@seamustuohy.com>
;; Keywords: mail, icalendar, notmuch, gnus

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;


;;; Code:

(require 'ical-event)
(require 'ical-event-reply)
(require 'mail-calendar-org)
(require 'mm-decode)
(require 'notmuch-show)

(defgroup notmuch-calendar nil
  "Settings for inline display of iCalendar events."
  :group 'notmuch-article
  :prefix "notmuch-calendar-")

(defcustom notmuch-calendar-reply-bufname "*CAL*"
  "Buffer used for building iCalendar reply."
  :type '(string)
  :group 'notmuch-calendar)

(make-variable-buffer-local
 (defvar notmuch-calendar-reply-status nil))

(make-variable-buffer-local
 (defvar notmuch-calendar-event nil))

(make-variable-buffer-local
 (defvar notmuch-calendar-handle nil))

(defvar notmuch-calendar-identities
  (cl-mapcan (lambda (x) (if (listp x) x (list x)))
             (list user-full-name (regexp-quote user-mail-address)
                   ; NOTE: this one can be a list
                   ; NOTE: this variable works even if you don't use gnus
                   gnus-ignored-from-addresses)))

;; TODO: make the template customizable
(defmethod ical-event->notmuch-calendar ((event ical-event) &optional reply-status)
  "Format an overview of EVENT details."
  (with-slots (organizer summary description location recur uid method rsvp) event
    (let ((headers `(("Summary" ,summary)
                     ("Location" ,location)
                     ("Time" ,(ical-event:org-timestamp event))
                     ("Organizer" ,organizer)
                     ("Method" ,method))))

      (when (and (not (ical-event-reply-p event)) rsvp)
        (setq headers (append headers
                              `(("Status" ,(or reply-status "Not replied yet"))))))

      (concat
       (apply #'concat (mapcar (lambda (x)
                                 (format "%-12s%s\n"
                                         (propertize (concat (car x) ":") 'face 'bold)
                                         (cadr x)))
                               headers))
       "\n"
       description))))

(defmacro with-decoded-handle (handle &rest body)
  "Execute BODY in buffer containing the decoded contents of HANDLE."
  (let ((charset (make-symbol "charset")))
    `(let ((,charset (cdr (assoc 'charset (mm-handle-type ,handle)))))
       (with-temp-buffer
         (mm-insert-part ,handle)
         (when (string= ,charset "utf-8")
           (mm-decode-coding-region (point-min) (point-max) 'utf-8))

         ,@body))))


(defun ical-event-from-handle (handle &optional attendee-name-or-email)
  (with-decoded-handle handle
      (ical-event-from-buffer (current-buffer) attendee-name-or-email)))

(defun notmuch-calendar-send-buffer-by-mail (buffer-name subject id)
  "TODO Write function documentation here"
  (let ((message-signature nil))
    (with-temp-buffer
      ;; Open the message with the corresponding ID
      (notmuch-show id nil (current-buffer) id "*ICAL-REPLY*")
      ;; Create a reply message
      (notmuch-show-reply)
      (message-goto-body)
      (mml-insert-multipart "alternative")
      (mml-insert-empty-tag 'part 'type "text/html")
      (mml-attach-buffer buffer-name "text/calendar; method=REPLY; charset=UTF-8")
      (message-goto-subject)
      (delete-region (line-beginning-position) (line-end-position))
      (insert "Subject: " subject)
      (message-send-and-exit)
      ;; Kill the email message buffer
      (notmuch-bury-or-kill-this-buffer))))

(defun notmuch-calendar-reply (data)
  (let* ((handle (first data))
         (status (second data))
         (event (third data))
         (message-id (notmuch-show-get-message-id))
         (reply (with-decoded-handle handle
                   (ical-event-reply-from-buffer (current-buffer)
                                                 status notmuch-calendar-identities))))

    (when reply
      (cl-flet ((fold-icalendar-buffer ()
                  (goto-char (point-min))
                  (while (re-search-forward "^\\(.\\{72\\}\\)\\(.+\\)$" nil t)
                    (replace-match "\\1\n \\2")
                    (goto-char (line-beginning-position)))))
        (let ((subject (concat (capitalize (symbol-name status))
                               ": " (ical-event:summary event))))

          (with-current-buffer (get-buffer-create notmuch-calendar-reply-bufname)
            (delete-region (point-min) (point-max))
            (insert reply)
            (fold-icalendar-buffer)
            (notmuch-calendar-send-buffer-by-mail (buffer-name) subject message-id))

          ;; Back in article buffer
          (setq-local notmuch-calendar-reply-status status)
          (when mail-calendar-org-enabled-p
            (notmuch-calendar:org-event-update event status)
            ;; refresh message buffer to update the reply status
            (with-current-buffer (current-buffer)
              (notmuch-show-refresh-view))))))))

(defun notmuch-calendar-sync-event-to-org (event)
  (cal-event:sync-to-org event notmuch-calendar-reply-status))

(defun notmuch-show-insert-part-text/calendar (msg part content-type nth depth button)
  "Modified from notmuch-show.el"
  (insert (with-temp-buffer
            (insert (notmuch-get-bodypart-text msg part notmuch-show-process-crypto))
            ;; Insert calendaring functionality buttons
            (notmuch-calendar-mm-inline)
            ;; notmuch-get-bodypart-text does no newline conversion.
            ;; Replace CRLF with LF before icalendar can use it.
            (goto-char (point-min))
            (while (re-search-forward "\r\n" nil t)
              (replace-match "\n" nil nil))
            (let ((file (make-temp-file "notmuch-ical"))
                  result)
              (unwind-protect
                  (progn
                    (unless (icalendar-import-buffer file t)
                      (error "Icalendar import error. See *icalendar-errors* for more information"))
                    (set-buffer (get-file-buffer file))
                    (setq result (buffer-substring (point-min) (point-max)))
                    (set-buffer-modified-p nil)
                    (kill-buffer (current-buffer)))
                (delete-file file))
              result)))
  t)


(defun notmuch-calendar-mm-inline (handle)
  (let ((event (ical-event-from-handle handle notmuch-calendar-identities))
        (reply-status "Not replied yet")
        reply-buttons
        org-buttons)

    (setq notmuch-calendar-reply-status nil)

    (when event
      (when (and (not (ical-event-reply-p event))
                 (ical-event:rsvp event))
        (when mail-calendar-org-enabled-p
          (setq reply-status (or (notmuch-calendar:org-event-reply-status event)
                                 reply-status)))

        (setq reply-buttons
              `(("Accept" notmuch-calendar-reply (,handle accepted ,event ))
                ("Tentative" notmuch-calendar-reply (,handle tentative ,event ))
                ("Decline" notmuch-calendar-reply (,handle declined ,event )))))

      (when mail-calendar-org-enabled-p
        (let* ((org-entry-exists-p (notmuch-calendar:org-entry-exists-p event))
               (export-button-text (if org-entry-exists-p "Update Org Entry" "Export to Org")))

          (setq org-buttons (append org-buttons
                                `(("Show Agenda" notmuch-calendar-show-org-agenda ,event))))

          (when (ical-event-request-p event)
            (setq org-buttons (append org-buttons
                                  `((,export-button-text notmuch-calendar-sync-event-to-org ,event)))))
          (when org-entry-exists-p
            (setq org-buttons (append org-buttons
                                  `(("Show Org Entry" notmuch-calendar-show-org-entry ,event)))))))

      (cl-flet ((insert-button-group (buttons)
                  (when buttons
                    (mapc (lambda (x)
                            (apply 'notmuch-calendar-insert-button x)
                            (insert "    "))
                          buttons)
                    (insert "\n\n"))))

        (insert-button-group reply-buttons)
        (insert-button-group org-buttons))

      (setq notmuch-calendar-event event
            notmuch-calendar-handle handle)
      (insert (ical-event->notmuch-calendar event reply-status)))))

(defun notmuch-calendar-save-part (handle)
  (let (event)
    (when (and (equal (car (mm-handle-type handle)) "text/calendar")
               (setq event (ical-event-from-handle handle notmuch-calendar-identities)))

      (cal-event:sync-to-org event))))

(defun notmuch-calendar-save-event ()
  "Save the Calendar event in the text/calendar part under point."
  (interactive)
  ;; Create a part handle for the calendar object notmuch-part
  (let ((data (notmuch-show-current-part-handle)))
    (when data
      (notmuch-calendar-save-part data))))

(defun notmuch-calendar-reply-accept ()
  "Accept invitation in the current article."
  (interactive)
  (with-current-buffer (current-buffer)
    (notmuch-calendar-reply (list notmuch-calendar-handle 'accepted notmuch-calendar-event))
    (setq-local notmuch-calendar-reply-status 'accepted)))

(defun notmuch-calendar-reply-tentative ()
  "Send tentative response to invitation in the current article."
  (interactive)
  (with-current-buffer (current-buffer)
    (notmuch-calendar-reply (list notmuch-calendar-handle 'tentative notmuch-calendar-event))
    (setq-local notmuch-calendar-reply-status 'tentative)))

(defun notmuch-calendar-reply-decline ()
  "Decline invitation in the current article."
  (interactive)
  (with-current-buffer (current-buffer)
    (notmuch-calendar-reply (list notmuch-calendar-handle 'declined notmuch-calendar-event))
    (setq-local notmuch-calendar-reply-status 'declined)))

(defun notmuch-calendar-event-export ()
  "Export calendar event to `org-mode', or update existing agenda entry."
  (interactive)
  (with-current-buffer (current-buffer)
    (notmuch-calendar-sync-event-to-org notmuch-calendar-event))
  ;; refresh article buffer in case the reply had been sent before initial org
  ;; export
  (with-current-buffer (current-buffer)
    (gnus-summary-show-article)))

(defun notmuch-calendar-event-show ()
  "Display `org-mode' agenda entry related to the calendar event."
  (interactive)
  (notmuch-calendar-show-org-entry
   (with-current-buffer (current-buffer)
     notmuch-calendar-event)))

(defun notmuch-calendar-event-check-agenda ()
  "Display `org-mode' agenda for days between event start and end dates."
  (interactive)
  (notmuch-calendar-show-org-agenda
   (with-current-buffer (current-buffer)
     notmuch-calendar-event)))


(defvar notmuch-calendar-part-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" notmuch-calendar-reply-accept)
    (define-key map "t" notmuch-calendar-reply-tentative)
    (define-key map "d" notmuch-calendar-reply-decline)
    (define-key map "c" notmuch-calendar-event-check-agenda)
    (define-key map "e" notmuch-calendar-event-export)
    (define-key map "s" notmuch-calendar-event-show))
    map)
  "Submap for part commands")
(fset 'notmuch-show-part-map notmuch-show-part-map)

;; Add a key "i" to the notmuch show mode map which opens the calendar map
(define-key notmuch-show-mode-map "i" 'notmuch-calendar-part-map)

(provide 'notmuch-calendar)
;;; notmuch-calendar.el ends here
