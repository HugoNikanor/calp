(define-module (hnh util ascii)
  :use-module (hnh util)
  :use-module (srfi srfi-88)
  :export (ascii-upcase
           ascii-upcase-char
           string-ascii-ci=?
           string-ascii-contains-ci
           ))

(define (ascii-upcase-char c)
  (define ascii-lowers (char-set-intersection char-set:ascii char-set:lower-case))
  (define ascii-upcase-offset (- (char->integer #\A) (char->integer #\a)))
  (if (char-set-contains? ascii-lowers c)
      (-> c char->integer (+ ascii-upcase-offset) integer->char)
      c))

(define (ascii-upcase str)
  (string-map ascii-upcase-char str))

(define (string-ascii-ci=? a b)
  (string=? (ascii-upcase a)
            (ascii-upcase b)))


(define* (string-ascii-contains-ci
          haystack needle
          optional:
          (start1 0) (end1 (string-length haystack))
          (start2 0) (end2 (string-length needle)))
  (string-contains
   (ascii-upcase (substring/shared haystack start1 end1))
   (ascii-upcase (substring/shared needle   start2 end2))))
