(define-module (hnh util language)
  :export (resolve-language))


;; Locale objects, such as %global-locale, doesn't provide a way to access the language name,
;; This is for procedures which want to handle their translations manually.
(define (resolve-language)
  "Returns a two character symbol representing the \"current\" language. e.g. en"
  (string->symbol
   (string-take
    (or (getenv "LC_MESSAGES")
        (getenv "LC_ALL")
        "en")
    2)))
