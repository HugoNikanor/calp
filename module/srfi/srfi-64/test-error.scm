;; Copyright © 2022 Hugo Hörnquist
;; Copyright for this file, however, majority of contents borrowed under the
;; below mentioned license agreement from srfi/srfi-64/testing.scm of Guile 2.2.7.

;; Copyright (c) 2005, 2006, 2007, 2012, 2013 Per Bothner
;; Added "full" support for Chicken, Gauche, Guile and SISC.
;;   Alex Shinn, Copyright (c) 2005.
;; Modified for Scheme Spheres by Álvaro Castro-Castilla, Copyright (c) 2012.
;; Support for Guile 2 by Mark H Weaver <mhw@netris.org>, Copyright (c) 2014.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;; Commentary:
;; The code is directly copied from Guile's source tree
;; (module/srfi/srfi-64/testing.scm), but @var{etype}
;; is passed to @code{catch}, causing it to actually
;; check the expected error.
;;; Code:

(define-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-64)
  :use-module (hnh util)
  :replace (test-error))

(define %test-source-line2  (@@ (srfi srfi-64) %test-source-line2))
(define %test-on-test-begin (@@ (srfi srfi-64) %test-on-test-begin))
(define %test-on-test-end   (@@ (srfi srfi-64) %test-on-test-end))
(define %test-report-result (@@ (srfi srfi-64) %test-report-result))

(define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (cond ((%test-on-test-begin r)
              (let ((et etype))
                (test-result-set! r 'expected-error et)
                (%test-on-test-end r
                                   (catch etype
                                     (lambda ()
                                       (test-result-set! r 'actual-value expr)
                                       #f)
                                     (lambda (key . args)
                                       ;; TODO: decide how to specify expected
                                       ;; error types for Guile.
                                       (test-result-set! r 'actual-error
                                                         (cons key args))
                                       #t)))
                (%test-report-result)))))))

(define-syntax test-error
    (lambda (x)
      (syntax-case (list x (list (syntax quote) (%test-source-line2 x))) ()
	(((mac tname etype expr) line)
	 (syntax
	  (let* ((r (test-runner-get))
		 (name tname))
	    (test-result-alist! r (cons (cons 'test-name tname) line))
	    (%test-error r etype expr))))
	(((mac etype expr) line)
	 (syntax
	  (let* ((r (test-runner-get)))
	    (test-result-alist! r line)
	    (%test-error r etype expr))))
	(((mac expr) line)
	 (syntax
	  (let* ((r (test-runner-get)))
	    (test-result-alist! r line)
	    (%test-error r #t expr)))))))

