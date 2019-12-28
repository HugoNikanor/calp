#!/bin/bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

. $(dirname $(dirname $(realpath $0)))/env

exec guile -e main -s $0 "$@"
!#

(use-modules (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-41)
             (srfi srfi-41 util)
             (srfi srfi-88)             ; keyword syntax

             (util)
             (util io)

             ((entry-points html)     :prefix     html-)
             ((entry-points terminal) :prefix terminal-)
             ((entry-points import)   :prefix   import-)
             ((entry-points text)     :prefix     text-)
             ((entry-points info)     :prefix     info-)
             ((entry-points ical)     :prefix     ical-)

             ((entry-points server)   :prefix   server-)

             (ice-9 getopt-long)

             (statprof)

             (parameters))

(define options
  '((mode (value #t) (single-char #\m))
    (output (value #t) (single-char #\o))
    (statprof (value optional))))

(define (ornull a b)
  (if (null? a)
      b a))

(define (main args)
  (define opts (getopt-long args options #:stop-at-first-non-option #t))
  (define stprof (option-ref opts 'statprof #f))

  (when stprof
    (statprof-start))

  (primitive-load (format #f "~a/.config/calp/config.scm"
                          (getenv "HOME")))

  (with-output-to-port
      (open-output-port (option-ref opts 'output "-"))
    (lambda ()
      (let ((ropt (ornull (option-ref opts '() '())
                          '("term"))))
        ((case (string->symbol (car ropt))
           ((html)       html-main)
           ((term)   terminal-main)
           ((import)   import-main)
           ((text)       text-main)
           ((info)       info-main)
           ((ical)       ical-main)
           ((server)   server-main)
           (else => (lambda (s) (error "Unsupported mode of operation:" s))))
         ropt))
      (newline)))

  (when stprof
    (statprof-stop)
    (statprof-display (current-error-port)
                      style: (if (boolean? stprof)
                                 'flat
                                 (string->symbol stprof)))))
