(define-module (text numbers)
  :export (number->string-cardinal
           number->string-ordinal
           each-string))

(define (get mod-symb proc-symb)
  (module-ref (resolve-interface `(text numbers ,mod-symb))
              proc-symb))

;; "sv_SE.UTF-8"
(define (resolve-language)
  (string->symbol
   (string-take
    (or (getenv "LC_MESSAGES")
        (getenv "LC_ALL")
        "en")
    2)))

(define* (number->string-cardinal
          n #:optional (language (resolve-language)))
  ((get language 'number->string-cardinal) n))

(define* (number->string-ordinal
          n #:optional (language (resolve-language)))
  ((get language 'number->string-ordinal) n))

;; TODO change API to allow language, and stop having random extra
;; arguments for implementations.
(define* (each-string count . args)
  (define language (resolve-language))
  (apply (get language 'each-string)
         count args))

;; scheme@(guile-user)> (number->string-cardinal 123)
;; $10 = "hundratjugotre"
;; scheme@(guile-user)> (number->string-ordinal 123)
;; $11 = "hundratjugotredje"
;; scheme@(guile-user)> (each-string 10)
;; $12 = "var tionde"
