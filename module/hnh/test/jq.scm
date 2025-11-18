(define-module (hnh test jq)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((rnrs io ports) :select (get-string-all))
  :use-module ((hnh util) :select (begin1))
  ;; :use-module (json)
  :export (jq))


(define (jq str filter)
  (let ((in* out (car+cdr (pipe)))
        (in out* (car+cdr (pipe))))
    (define pid
     (spawn "jq" (list "jq" filter)
            input: in*
            output: out*))
    (close-port in*)
    (close-port out*)

    (display str out)
    (force-output out)
    (close-port out)

    (begin1 (get-string-all in)
            (close-port in))))
