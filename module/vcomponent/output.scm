(define-module (vcomponent output)
  #:use-module (vcomponent)
  #:use-module (vcomponent control)
  #:use-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (datetime)
  #:use-module (datetime util)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:export (serialize-vcomponent))


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

(define* (serialize-vcomponent comp #:optional (port (current-output-port)))
  "Recursively write a component back to its ICS form.
Removes the X-HNH-FILENAME attribute, and sets PRODID to
\"HugoNikanor-calparse\" in the output."
  (with-replaced-attrs
   (comp (prodid "HugoNikanor-calparse"))

   (format port "BEGIN:~a~%" (type comp))

   (let ((kvs (map (lambda (key) (list key (attr comp key)))
                   (attribute-keys comp))))
     (for kv in kvs
          (let* (((key value) kv))
            (catch 'wrong-type-arg
              (lambda ()
                (format port "~a~:{;~a=~a~}:~a~%"
                        key
                        (properties (attr* comp key))
                        (string->ics-safe-string
                         (case key
                           ((DTSTART DTEND)
                            (cond [(string? value) value]
                                  [(date? value) (date->string value "~Y~m~d")]
                                  [(datetime? value)
                                   (datetime->string value)]))
                           ((X-HNH-DURATION)
                            (format #f "~s" value))
                           (else value)))))

              ;; Catch
              (lambda (type proc fmt . args)
                (apply format (current-error-port) "[ERR] ~a in ~a (~a) ~a:~%~?~%"
                       type key proc (attr comp 'X-HNH-FILENAME)
                       fmt args)))))

     (for-each (cut serialize-vcomponent <> port) (children comp)))
   (format port "END:~a~%" (type comp))))
