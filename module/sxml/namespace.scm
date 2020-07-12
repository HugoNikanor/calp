(define-module (sxml namespace)
  :use-module (util)
  :use-module (sxml transform))

(define* (symbol-split symbol key: (sep #\:))
  (->> (-> symbol
           symbol->string
           (string-split sep))
       (map string->symbol)
       (apply values)))

(define (string-last string)
  (string-ref string (1- (string-length string))))

;; sxml, (U symbol string #f) → sxml
;; NOTE possibly allow namespace to be a map between namespaces
(define-public (move-to-namespace sxml namespace)

  (define nssymb
    (cond
     [(not namespace) '#{}#]
     [(symbol? namespace)
      (if (char=? #\: (string-last (symbol->string namespace)))
          namespace (symbol-append namespace ':))]
     [(string? namespace)
      (if (char=? #\: (string-last namespace))
          (string->symbol namespace)
          (string->symbol (string-append namespace ":")))]))

  (define (ns tag)
    (call-with-values (lambda () (symbol-split tag))
      (case-lambda
        [(ns tag) (symbol-append nssymb tag)]
        [(tag) (symbol-append nssymb tag)])))

  (pre-post-order
   sxml
   `((*TOP* . ,list) (*PI* . ,list) (*text* . ,(lambda (tag text) text))
     (*default* . ,(lambda (tag . body) `(,(ns tag) . ,body))))))
