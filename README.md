ical-event
==========

Sync iCalendar event invitations from notmuch to Org agenda.

Setup
=====

    (add-to-list 'load-path "/path/to/ical-event")
    (require 'notmuch-calendar)
    (mail-calendar-org-setup)

    ;; to enable optional iCalendar->Org sync functionality
    ;; NOTE: both the capture file and the headline(s) inside must already exist
    (setq mail-calendar-org-capture-file "~/org/agenda.org")
    (setq mail-calendar-org-capture-headline '("Calendar"))
    (mail-calendar-org-setup)


