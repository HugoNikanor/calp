(define-module (hnh test util)
  :use-module ((hnh util) :select (begin1))
  :use-module ((hnh util io) :select (call-with-tmpfile))
  :use-module (ice-9 pretty-print)
  :use-module ((ice-9 rdelim) :select (read-string))
  :use-module ((ice-9 popen)
               :select (open-pipe*
                        close-pipe))
  :export (µs
           transform-time-of-day
           green
           red
           yellow
           bold
           make-indent
           string-replace-head
           diff
           ))

(define (µs x)
  (* x #e1e6))

(define (transform-time-of-day tod)
  (+ (* (µs 1) (car tod))
     (cdr tod)))

(define (escaped sequence string)
  (format #f "\x1b[~am~a\x1b[m" sequence string))

;; Terminal output formatting. Doesn NOT compose
(define (green s)  (escaped 32 s))
(define (red s)    (escaped 31 s))
(define (yellow s) (escaped 33 s))
(define (bold s)   (escaped  1 s))

(define (make-indent depth)
  (make-string (* 2 depth) #\space))

(define (string-replace-head s1 s2)
  (string-replace s1 s2
                  0 (string-length s2)))


(define diff-cmd
  ;; '("diff")
  '("git" "diff" "--no-index" "--color-moved=default" "--color=always"; "--word-diff=color"
    )
  )

(define (diff s1 s2)
  (let ((filename1 (call-with-tmpfile (lambda (p f) (pretty-print s1 p display?: #t) f)))
        (filename2 (call-with-tmpfile (lambda (p f) (pretty-print s2 p display?: #t) f))))
    (let ((pipe (apply open-pipe*
                 OPEN_READ
                 (append diff-cmd (list filename1 filename2)))))
      (begin1 (read-string pipe)
              (close-pipe pipe)))))
