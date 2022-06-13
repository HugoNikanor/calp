#!/usr/bin/bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-
here=$(dirname $(realpath $0))

. "$(dirname "$(dirname "$here")")/env"

exec $GUILE -e main -s "$0" -- "$@"
!#

(use-modules (sxml simple)
             ((sxml xpath) :select (sxpath))
             (sxml match)
             (rnrs lists)
             (ice-9 regex)
             (ice-9 popen)
             (ice-9 format)
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
                          #:trim-whitespace? #t
                          #:namespaces
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
                     (for-each display-entry (cadr group)))
                   (group-by-file filtered-data))
         (exit 1)))))
