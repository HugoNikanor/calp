(define-module (vcalendar output)
  #:use-module (vcalendar)
  #:use-module (vcalendar control)
  #:use-module (util)
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:export (print-vcomponent
            serialize-vcomponent
            color-if
            STR-YELLOW STR-RESET))

(define STR-YELLOW "\x1b[0;33m")
(define STR-RESET "\x1b[m")

(define-syntax-rule (color-if pred color body ...)
  (let ((pred-value pred))
    (format #f "~a~a~a"
            (if pred-value color "")
            (begin body ...)
            (if pred-value STR-RESET ""))))

(define* (print-vcomponent comp #:optional (depth 0))
  (let ((kvs (map (lambda (key) (cons key (attr comp key)))
                  (attributes comp))))
    (format #t "~a <~a> :: ~:a~%"
            (make-string depth #\:)
            (type comp) comp)
    (for-each-in kvs
                 (lambda (kv)
                   (let ((key (car kv))
                         (value (cdr kv)))
                     (format #t "~a ~20@a: ~a~%"
                             (make-string depth #\:)
                             key value))))
    (for-each-in (children comp)
                 (cut print-vcomponent <> (1+ depth)))))



;;; TODO
;; Error in CREATED /home/hugo/.calendars/b85ba2e9-18aa-4451-91bb-b52da930e977/a1a25238-d63d-46a1-87fd-d0c9334a7a30CalSync.ics:
;; Wrong type argument in position 1 (expecting string): ("20180118T124015Z" "VALARM")

(define (string->ics-safe-string str)
  "TODO wrap at 75(?) columns."
  (define (escape char)
    (string #\\ char))

  (string-concatenate
   (map (lambda (c)
          (case c
            ((#\newline) "\\n")
            ((#\, #\; #\\) => escape)
            (else => string)))
        (string->list str))))

;;; TODO parameters ( ;KEY=val: )
(define* (serialize-vcomponent comp #:optional (port (current-output-port)))
  "Recursively write a component back to its ICS form.
Removes the X-HNH-FILENAME attribute, and sets PRODID to
\"HugoNikanor-calparse\" in the output."
  (with-replaced-attrs
   (comp (prodid "HugoNikanor-calparse"))

   (format port "BEGIN:~a~%" (type comp))
   (let ((kvs (map (lambda (key) (list key (attr comp key)))
                   (filter (negate (cut key=? <> 'X-HNH-FILENAME))
                           (attributes comp)))))
     (for-each-in
      kvs (lambda (kv)
            (let* (((key value) kv))
              (catch 'wrong-type-arg
                (lambda ()
                  (format port "~a:~a~%" key
                          (string->ics-safe-string
                           (or (case key
                                 ((DTSTART DTEND)
                                  (if (string? value)
                                      value
                                      (time->string value "~Y~m~dT~H~M~S")))

                                 ((DURATION) "Just forget it")

                                 (else value))
                               ""))))

                ;; Catch
                (lambda (type proc fmt . args)
                  (apply format (current-error-port) "[ERR] ~a in ~a (~a) ~a:~%~?~%"
                         type key proc (attr comp 'X-HNH-FILENAME)
                         fmt args))))))

     (for-each (cut serialize-vcomponent <> port) (children comp)))
   (format port "END:~a~%" (type comp))))