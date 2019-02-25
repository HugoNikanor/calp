#!/usr/bin/guile \
-s
!#

(begin
  ;; Supurflous begin block here to make sourcing into geiser easier.
  (setenv "LD_LIBRARY_PATH" (getcwd))
  (load-extension "libguile-calendar" "init_vcomponent"))

(define root (make-vcomponent "testcal/d1-b.ics"))
(define cal (car (vcomponent-children root)))

;; TODO flatten all calendars into root

(use-modules (srfi srfi-19)
             (srfi srfi-26))

(for-each (lambda (ev)
            (vcomponent-set-attribute!
             ev "DTSTART"
             ((cut string->date <> "~Y~m~dT~H~M~S")
              (vcomponent-get-attribute ev "DTSTART"))))
          (vcomponent-children cal))

(display (vcomponent-get-attribute (car (vcomponent-children cal))
                                   "DTSTART"))
(newline)
(display (vcomponent-get-attribute (car (vcomponent-children cal))
                                   "DTSTART"))
(newline)
