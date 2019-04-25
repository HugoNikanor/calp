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
             (output none)

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
  '((mode (value #t) (single-char #\m))
    (statprof (value #f))))

(define (ornull a b)
  (if (null? a)
      b a))

(define (main args)
  (let ((opts (getopt-long args options #:stop-at-first-non-option #t)))
    ((lambda (thunk) (if (option-ref opts 'statprof #f)
                    ((@ (statprof) statprof)
                     thunk
                     #:count-calls? #t
                     #:display-style 'tree)
                    (thunk)))
     (lambda ()
       (init
        (lambda (c e)
          (let ((ropt (ornull (option-ref opts '() '())
                              '("term"))))
            ((case (string->symbol (car ropt))
               ((none) none-main)
               ((html) html-main)
               ((term) terminal-main))
             c e ropt))))))
    (newline)))
