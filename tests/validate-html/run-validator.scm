#!/usr/bin/bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-
root=$(dirname "$(dirname "$(dirname "$(realpath "$0")")")")

eval "$(env __PRINT_ENVIRONMENT=1 ${root}/calp)"

exec $GUILE -e main -s "$0" -- "$@"
!#

(unless (getenv "CALP_TEST_ENVIRONMENT")
  (format (current-error-port) "Not running in test environment, abandoning~%")
  (exit 1))

(use-modules (sxml simple)
             ((sxml xpath) :select (sxpath))
             (sxml match)
             (rnrs lists)
             (ice-9 regex)
             (ice-9 popen)
             (ice-9 format)
             (srfi srfi-88
             ((hnh util) :select (group-by ->)))

(define (error-string error)
 (cond (((sxpath '(// nu:message)) error)
        (negate null?) => (compose sxml->string car))
       (else "")))

(define (ignore-rule error)
  (string-match "Element (calendar|icalendar) not allowed as child"
                (error-string error)))

(define (group-by-file entries)
  (group-by (sxpath '(// @ url))
            entries))

(define (display-entry entry)
  (sxml-match
   entry
   [(nu:error (@ (last-line ,last-line)
                 (first-column ,first-column)
                 (last-column ,last-column))
              (nu:message ,msg ...)
              (nu:extract ,extract ...))
    (format #t "  - ERROR - ~a:~a-~a - ~a - ~a~%"
            last-line first-column last-column
            (sxml->string `(nu:message ,@msg))
            (sxml->string `(nu:extract ,@extract)))]

   [(nu:info (@ (last-line ,last-line)
                (first-column ,first-column)
                (last-column ,last-column)
                (type ,type))
             (nu:message ,msg ...)
             (nu:extract ,extract ...))
    (format #t "  - ~5a - ~a:~a-~a - ~a - ~a~%"
            type last-line first-column last-column
            (sxml->string `(nu:message ,@msg))
            (sxml->string `(nu:extract ,@extract)))]))

(define (main args)
  (define pipe (open-pipe* OPEN_READ "html5validator"
                           "--format" "xml"
                           ;; "--verbose"
                           "--show-warnings"
                           "--"
                           "selenium.xhtml"
                           "raw.xhtml"
                           ))
  (define data (xml->sxml pipe
                          trim-whitespace?: #t
                          namespaces:
                          '((nu . "http://n.validator.nu/messages/")
                            (xhtml . "http://www.w3.org/1999/xhtml"))))
  (close-pipe pipe)
  (let ((filtered-data
          (filter (negate ignore-rule)
                  ((sxpath '(// nu:messages *)) data))))
    (if (null? filtered-data)
        (begin
          (display "Everything fine!")
          (newline)
          (exit 0))
        (begin
         (for-each (lambda (group)
                     (format #t "~a~%" (-> group car (assoc-ref 'url) car))
                     (for-each display-entry (cdr group)))
                   (group-by-file filtered-data))
         (exit 1)))))
