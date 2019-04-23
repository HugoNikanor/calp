#!/usr/bin/guile -s
!#

#|
 | Example file which reads my regular calendar, filters it down to only
 | the events between specific times, and prints that calendar in ICS
 | format to standard output.
 |#

(add-to-load-path (dirname (current-filename)))

(use-modules (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (vcomponent)
             (vcomponent datetime)
             (vcomponent output)
             (util))


(begin
  ;; (define *path* "/home/hugo/.calendars/b85ba2e9-18aa-4451-91bb-b52da930e977/")
  (define *path* "/home/hugo/.calendars/D1/")
  (define cal (make-vcomponent *path*)))

(filter-children!
 (lambda (ev) (and (eq? 'VEVENT (type ev))
              (event-in? ev (date->time-utc (string->date "2019-04-03" "~Y-~m-~d")))))
 cal)

(serialize-vcomponent cal)
