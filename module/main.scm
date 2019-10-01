#!/bin/bash
# -*- mode: scheme -*-

root=$(dirname $(dirname $(realpath $0)))

GUILE_LOAD_PATH="$root/module:$GUILE_LOAD_PATH"
GUILE_LOAD_COMPILED_PATH="$root/obj/module:$GUILE_LOAD_COMPILED_PATH"
LD_LIBRARY_PATH="$root/lib:$LD_LIBRARY_PATH"

export GUILE_LOAD_PATH GUILE_LOAD_COMPILED_PATH LD_LIBRARY_PATH
export GUILE_AUTO_COMPILE=0

exec guile -e main -s $0 "$@"
!#

(use-modules (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (srfi srfi-41)
             (srfi srfi-41 util)
             (srfi srfi-88)
             (util)
             (vcomponent)
             (vcomponent recurrence)
             (vcomponent datetime)

             (output html)
             (output terminal)
             (output none)
             (output text)
             (output import)
             (output info)
             (server)

             (ice-9 getopt-long)

             (statprof)

             (parameters)
             (config))

;; Reads all calendar files from disk, and creates a list of "regular" events,
;; and a stream of "repeating" events, which are passed in that order to the
;; given procedure @var{proc}.
;;
;; Given as a sepparate function from main to ease debugging.
(define* (init proc #:key (calendar-files (calendar-files)))
  (define calendars (map make-vcomponent calendar-files))
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
    (file (value #t) (single-char #\f))
    (output (value #t) (single-char #\o))
    (format (value #f))
    (width (value #t) (single-char #\w))
    (statprof (value optional))))

(define (ornull a b)
  (if (null? a)
      b a))

(define (main args)
  (define opts (getopt-long args options #:stop-at-first-non-option #t))
  (define stprof (option-ref opts 'statprof #f))

  (when stprof
    (statprof-start))

  (with-output-to-port (open-output-port (option-ref opts 'output "-"))
    (lambda ()
      (if (option-ref opts 'format #f)
          (for-each (lambda (l) (display l) (newline))
                    (flow-text
                     (with-input-from-port (open-input-port (option-ref opts 'file "-"))
                       (@ (ice-9 rdelim) read-string))
                     #:width (or (string->number (option-ref opts 'width "")) 70)))

          (init
           (lambda (c e)
             (let ((ropt (ornull (option-ref opts '() '())
                                 '("term"))))
               ((case (string->symbol (car ropt))
                  ((none) none-main)
                  ((html) html-main)
                  ((term) terminal-main)
                  ((import) import-main)
                  ((info) info-main)
                  ((server) server-main))
                c e ropt)))
           calendar-files: (or (and=> (option-ref opts 'file #f)
                                       list)
                                (calendar-files))))
      (newline)))

  (when stprof
    (statprof-stop)
    (statprof-display (current-error-port)
                      style: (if (boolean? stprof)
                                 'flat
                                 (string->symbol stprof)))))
