#!/usr/bin/guile \
-s
!#

(begin
  ;; Supurflous begin block here to make sourcing into geiser easier.
  (setenv "LD_LIBRARY_PATH" (getcwd))
  (load-extension "libguile-calendar" "init_lib"))

(define make-vcomponent %vcomponent-make)
(define children %vcomponent-children)
(define set-attr! %vcomponent-set-attribute!)
(define get-attr %vcomponent-get-attribute)

(define root (make-vcomponent "testcal/d1-b.ics"))
(define cal (car (children root)))

;; TODO flatten all calendars into root

(use-modules (srfi srfi-19)
             (srfi srfi-26))

(define (mutate-attr! ev field transformer)
  (set-attr! ev field
             (transformer
              (get-attr ev field))))

(for-each (cut mutate-attr! <> "DTSTART"
               (cut string->date <> "~Y~m~dT~H~M~S"))
          (children cal))

(display (get-attr (car (children cal))
                   "DTSTART"))
(newline)
(display (get-attr (car (children cal))
                   "DTSTART"))
(newline)
