(define-module (hnh test xmllint)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((rnrs io ports) :select (get-string-all))
  :use-module ((hnh util) :select (begin1))
  :export (xmllint)
  )


(define (xmllint str)
  (let ((in* out (car+cdr (pipe)))
        (in out* (car+cdr (pipe)))
        (cmdline (string-split "xmllint --format -" #\space)))
    (define pid
     (spawn (car cmdline) cmdline
            input: in*
            output: out*))
    (close-port in*)
    (close-port out*)

    (display str out)
    (force-output out)
    (close-port out)

    (begin1 (get-string-all in)
            (close-port in))))
