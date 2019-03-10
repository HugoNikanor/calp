#!/usr/bin/guile -s
!#

#|
 | Example file which reads my regular calendar, filters it down to only
 | the events between specific times, and prints that calendar in ICS
 | format to standard output.
 |#

(add-to-load-path (dirname (current-filename)))

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-19)
             (srfi srfi-19 util)
             (vcalendar)
             (vcalendar output)
             (util))


(begin
  (define *path* "/home/hugo/.calendars/b85ba2e9-18aa-4451-91bb-b52da930e977/")
  (define cal (make-vcomponent *path*)))

(filter-children!
 (lambda (comp)
   (if (not (eq? 'VEVENT (type comp)))
       #t
       (let ((stime (date->time-utc (string->date "2019-03-12T12:00" "~Y-~m-~dT~H:~M")))
             (etime (date->time-utc (string->date "2019-03-13T11:59" "~Y-~m-~dT~H:~M"))))
         (and (time<=? stime (attr comp "DTSTART"))
              (time<=? (attr comp "DTSTART") etime)))))
 cal)

(serialize-vcomponent cal)
