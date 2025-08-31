(define-module (text numbers)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((hnh util language) :select (resolve-language))
  :export (number->string-cardinal
           number->string-ordinal
           each-string))

(define (get mod-symb proc-symb)
  (module-ref (catch 'misc-error
                (lambda () (resolve-interface `(text numbers ,mod-symb)))
                (lambda (err proc fmt args data)
                  ;; Possibly check if the err message starts with
                  ;; "no code for module"
                  (resolve-interface '(text numbers en))))
              proc-symb))

(define* (number->string-cardinal
          n optional: (language (resolve-language))
          rest: extra-kvs)
  (apply (get language 'number->string-cardinal) n extra-kvs))

(define* (number->string-ordinal
          n
          optional: (language (resolve-language))
          rest: extra-kvs)
  (apply (get language 'number->string-ordinal) n extra-kvs))

(define* (each-string count
                      optional: (language (resolve-language))
                      rest: extra-kvs)
  (apply (get language 'each-string)
         count extra-kvs))
