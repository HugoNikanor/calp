(define-module (sxml namespace)
  :use-module (calp util)
  :use-module (sxml transform))

(define* (symbol-split symbol key: (sep #\:))
  (->> (-> symbol
           symbol->string
           (string-split sep))
       (map string->symbol)
       (apply values)))

(define (string-last string)
  (string-ref string (1- (string-length string))))


;; Change the namespace for all tags in a given SXML tree.
;; Takes either a fix namespace everything is moved to, or
;; an assoc list which maps input namespaces to output namespaces.
;; A namespace is a symbol, string, or #f for no namespace.
;; keys in the assoc list can't be strings.
;; @example
;; (move-to-namespace '(test) '((#f . NEW)))
;; => (NEW:test)
;; (move-to-namespace '(a:a (b:b)) '((a . b) (b . a)))
;; => (b:a (a:b))
;; (move-to-namespace '(a:a (b:b)) #f)
;; => (a (b))
;; (move-to-namespace '(a:a (b:b)) 'c)
;; => (c:a (c:b))
;; @end example
;; sxml, (U symbol string #f (alist (U #f symbol) (U symbol string #f))) → sxml
(define-public (move-to-namespace sxml namespace-map)

  (define (nssymb key)
    (define namespace
      (if (list? namespace-map)
          (cond ((assoc key namespace-map)
                 => cdr)
                (else key))
          namespace-map))

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
        [(ns tag) (symbol-append (nssymb ns) tag)]
        [(tag) (symbol-append (nssymb #f) tag)])))

  (pre-post-order
   sxml
   `((*TOP* . ,list) (*PI* . ,list) (*text* . ,(lambda (tag text) text))
     (*default* . ,(lambda (tag . body) `(,(ns tag) . ,body))))))
