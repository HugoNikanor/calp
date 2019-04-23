#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (srfi srfi-41)
             (srfi srfi-41 util)
             (util)
             (vcomponent)
             (vcomponent recurrence)
             (vcomponent datetime)

             (output html)
             (output terminal)

             (ice-9 getopt-long)

             (parameters)
             (config))

;; Reads all calendar files from disk, and creates a list of "regular" events,
;; and a stream of "repeating" events, which are passed in that order to the
;; given procedure @var{proc}.
;;
;; Given as a sepparate function from main to ease debugging.
(define (init proc)
  (define calendars (map make-vcomponent (calendar-files)))
  (define events (concatenate (map (cut children <> 'VEVENT) calendars)))

  (let* ((repeating regular (partition repeating? events)))

    (set! repeating (sort*! repeating time<? (extract 'DTSTART))
          regular   (sort*! regular   time<? (extract 'DTSTART)))

    (proc
     calendars
     (interleave-streams
      ev-time<?
      (cons (list->stream regular)
            (map generate-recurrence-set repeating))))))

(define options
  '((mode (value #t) (single-char #\m))))

(define (main args)
  (let ((opts (getopt-long args options #:stop-at-first-non-option #t)))
    (init
     (lambda (c e)
       (let ((ropt (option-ref opts '() '("term"))))
         ((case (string->symbol (car ropt))
            ((html) html-main)
            ((term) terminal-main))
          c e ropt))))
    (newline)))
