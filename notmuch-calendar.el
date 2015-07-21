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
                   "stuohy@internews.org"
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

(defun notmuch-calendar-sync-event-to-org (event)
  (cal-event:sync-to-org event notmuch-calendar-reply-status))

(defun notmuch-show-insert-part-text/calendar (msg part content-type nth depth button)
  ;; Create buffer
  (let ((event)
        (handle msg))
    (insert (with-temp-buffer
              ;; get ical event from message
              (with-temp-buffer
                ;; Add message in it
                (insert (notmuch-get-bodypart-text msg part notmuch-show-process-crypto))
                ;; notmuch-get-bodypart-text does no newline conversion.
                ;; Replace CRLF with LF before icalendar can use it.
                (goto-char (point-min))
                (while (re-search-forward "\r?\n" nil t)
                  (replace-match "\n" nil nil))
                (setq event (ical-event-from-buffer (current-buffer) notmuch-calendar-identities)))
              (setq notmuch-calendar-event event)
              (let ((reply-status "Not replied yet")
                    reply-buttons
                    org-buttons)
                (setq notmuch-calendar-reply-status nil)
                ;; Check if RSVP'ed and set that
                (when event
                  (when mail-calendar-org-enabled-p
                    (let* ((org-entry-exists-p (mail-calendar:org-entry-exists-p event))
                           (export-button-text (if org-entry-exists-p "Update Org Entry" "Export to Org")))

                      ;; Create buttons related to ORG
                      (setq org-buttons (append org-buttons
                                                `(("Show Agenda" mail-calendar-show-org-agenda ,event))))

                      ;;Add either "export" or "show" button
                      (when (ical-event-request-p event)
                        (setq org-buttons (append org-buttons
                                                  `((,export-button-text notmuch-calendar-sync-event-to-org ,event)))))
                      (when org-entry-exists-p
                        (setq org-buttons (append org-buttons
                                                  `(("Show Org Entry" mail-calendar-show-org-entry ,event)))))))

                  ;; insert button groups into the buffer
                  (notmuch-calendar-insert-button-group org-buttons))
                ;; insert the status of the current event
                (insert (ical-event->notmuch-calendar event reply-status))
                (setq result (buffer-substring (point-min) (point-max)))
                ;; (end insert function) to insert button text into original buffer
                result))))
  t)

(defun notmuch-calendar-insert-button-group (buttons)
  (when buttons
    (mapc (lambda (x)
            (apply 'notmuch-calendar-insert-text-button x)
            (insert "    "))
          buttons)
    (insert "\n\n")))

(defun notmuch-calendar-insert-text-button (label button-action &optional args)
   (if args
       (insert-text-button label
                           :type 'notmuch-button-type
                           'action `(lambda (args)
                                      (,button-action ,args)))
     (insert-text-button label
                         :type 'notmuch-button-type
                         'action `(lambda (args)
                                    (,button-action)))))

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
  (mail-calendar-show-org-entry
   (with-current-buffer (current-buffer)
     notmuch-calendar-event)))

(defun notmuch-calendar-event-check-agenda ()
  "Display `org-mode' agenda for days between event start and end dates."
  (interactive)
  (mail-calendar-show-org-agenda
   (with-current-buffer (current-buffer)
     notmuch-calendar-event)))

;; Still need to define this
;; (defvar notmuch-calendar-part-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "a" 'notmuch-calendar-reply-accept)
;;     (define-key map "t" 'notmuch-calendar-reply-tentative)
;;     (define-key map "d" 'notmuch-calendar-reply-decline)
;;     (define-key map "c" 'notmuch-calendar-event-check-agenda)
;;     (define-key map "e" 'notmuch-calendar-event-export)
;;     (define-key map "s" 'notmuch-calendar-event-show)
;;   map)
;;   "Submap for part commands")

;; (fset 'notmuch-calendar-part-map notmuch-calendar-part-map)

;; ;; Add a key "i" to the notmuch show mode map which opens the calendar map
;; (define-key notmuch-show-mode-map "I" 'notmuch-calendar-part-map)

(provide 'notmuch-calendar)
;;; notmuch-calendar.el ends here
