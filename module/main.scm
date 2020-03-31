#!/bin/bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

. $(dirname $(dirname $(realpath $0)))/env

exec guile -e main -s $0 "$@"
!#

(use-modules (srfi srfi-1)
             (srfi srfi-41)
             (srfi srfi-41 util)
             (srfi srfi-88)             ; keyword syntax

             (util)
             (util io)
             (util time)

             ((entry-points html)      :prefix      html-)
             ((entry-points terminal)  :prefix  terminal-)
             ((entry-points import)    :prefix    import-)
             ((entry-points text)      :prefix      text-)
             ((entry-points info)      :prefix      info-)
             ((entry-points ical)      :prefix      ical-)
             ((entry-points benchmark) :prefix benchmark-)

             ((entry-points server)   :prefix   server-)

             (ice-9 getopt-long)

             (statprof)

             (util config all))

(define options
  '((statprof (value optional))
    (help (single-char #\h))))

(define (ornull a b)
  (if (null? a)
      b a))

(define (wrapped-main args)
  (define opts (getopt-long args options #:stop-at-first-non-option #t))
  (define stprof (option-ref opts 'statprof #f))

  (when stprof
    (statprof-start))

  (let ((config-file (format #f "~a/.config/calp/config.scm"
                             (getenv "HOME"))))
    (when (file-exists? config-file)
     (primitive-load config-file)))

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
       ((benchmark) benchmark-main)
       (else => (lambda (s)
                  (format (current-error-port)
                          "Unsupported mode of operation: ~a~%"
                          s)
                  (exit 1))))
     ropt))

  (when stprof
    (statprof-stop)
    (statprof-display (current-error-port)
                      style: (if (boolean? stprof)
                                 'flat
                                 (string->symbol stprof)))))


(use-modules (system vm frame))

(define (main args)
  (report-time! "Program start")
  (with-throw-handler #t
    (lambda () (wrapped-main args))
    (lambda (err . args)
      (define stack (make-stack #t))
      (format
       (current-error-port)
       "bindings = (~a)~%"
       (with-output-to-string
         (lambda ()
           (let loop ((frame (stack-ref stack 0)))
             (when frame
               (format #t "~{~a~^ ~}" (map binding-name (frame-bindings frame)))
               (let ((event (and=> (frame-lookup-binding frame 'event)
                                   binding-ref)))
                 (when event
                   (format (current-error-port) "event = ~a~%" event)
                   ((@ (vcomponent output) serialize-vcomponent)
                    event (current-error-port))))

               (loop (frame-previous frame))))))))))
