#!/usr/bin/guile -s
!#

(define *dir* (dirname (dirname (current-filename))))
(define (path subdir)
  (string-append *dir* "/" subdir))

(add-to-load-path (path "module"))

(use-modules (srfi srfi-64)
             (vcalendar))

(define v (make-vcomponent (path "testdata/prop.ics")))

(test-begin "Proporty test")
(test-equal (prop v 'KEY 'A) '(("1")))
(test-equal (prop v 'KEY 'B) '(("2")))
(test-equal (prop v 'KEY 'C) '())

(test-equal (properties v 'KEY) '(A B))
(test-equal (properties v "KEY") '(A B))

(test-end "Proporty test")
