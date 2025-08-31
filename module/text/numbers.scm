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
          n
          key: language allow-other-keys:
          rest: extra-kvs)
  (apply (get (or language (resolve-language)) 'number->string-cardinal) n extra-kvs))

(define* (number->string-ordinal
          n
          key: language allow-other-keys:
          rest: extra-kvs)
  (apply (get (or language (resolve-language)) 'number->string-ordinal) n extra-kvs))

(define* (each-string count
                      key: language allow-other-keys:
                      rest: extra-kvs)
  (apply (get (or language (resolve-language)) 'each-string)
         count extra-kvs))
