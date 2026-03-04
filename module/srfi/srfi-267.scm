;;; Commentary:
;;; Code copief from the [SRFI 267 page][SRFI-267],
;;; and simply wrapped in a Guile module declaration.
;;;
;;; [SRFI-267]: https://srfi.schemers.org/srfi-267/srfi-267.html
;;;
;;; Copyright
;;;
;;; © 2025–2026 Peter McGoron
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice (including the
;;; next paragraph) shall be included in all copies or substantial
;;; portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;; Code:
(define-module (srfi srfi-267)
  :export ())

(define (read-raw-string port)
  ;; This parser starts reading after `"`.
  ;; In the given examples, the parser starts at the dot:
  ;;
  ;; #"."asdf""
  ;; #".--"#"()""--"
  (define (read-char* location)
    (let ((ch (read-char port)))
      (if (eof-object? ch)
          (error (list "eof in raw string literal" location))
          ch)))
  (define delimiter
    (do ((ch (read-char* 'delim) (read-char* 'delim))
         (acc '(#\") (cons ch acc)))
        ((char=? ch #\")
         (reverse (cons #\" acc)))))
  (call-with-port (open-output-string)
    (lambda (out)
      (define (read-delimiter n rest-of-delimiter)
        (if (null? rest-of-delimiter)
            (get-output-string out)
            (let ((ch (read-char* 'check)))
              (if (char=? ch (car rest-of-delimiter))
                  (read-delimiter (+ n 1) (cdr rest-of-delimiter))
                  (do ((n n (- n 1))
                       (delimiter delimiter (cdr delimiter)))
                      ((zero? n) (read-raw ch))
                    (write-char (car delimiter) out))))))
      (define (read-raw ch)
        (if (char=? ch (car delimiter))
            (read-delimiter 1 (cdr delimiter))
            (begin (write-char ch out)
                   (read-raw (read-char* 'read)))))
      (read-raw (read-char* 'read)))))

(cond-expand
  (chicken (import (chicken read-syntax))
           (set-sharp-read-syntax! #\" read-raw-string))
  (guile (read-hash-extend #\"
                           (lambda (_ port)
                             (read-raw-string port))))
  (else (error "your implementation is not supported")))

;; (define (test x y)
;;   (display (string=? x y)) (newline))

;; (test "" #"""")
;; (test "a" #""a"")
;; (test "\\" #""\"")
;; (test "\"" #"-"""-")
;; (test "\\\"" #"-"\""-")
;; (test "#\"()\"" #"-"#"()""-")
;; (test "#\"\"a\"\"" #"-"#""a"""-")
;; (test "ends with \\\"" #"-"ends with \""-")
;; (test "multiline\nstring" #""multiline
;; string"")
;; (test "\n    no whitespace stripping" #""
;;     no whitespace stripping"")
