(define-module (hnh util language)
  :export (resolve-language))

;;; Given a #f or a string, return #f if #f or an empty string is given,
;;; and the given string otherwise
(define (falsify-string s)
  (cond ((and (boolean? s) (not s)) s)
        ((string-null? s) #f)
        (else s)))

;; Locale objects, such as %global-locale, doesn't provide a way to access the language name,
;; This is for procedures which want to handle their translations manually.
(define (resolve-language)
  "Returns a two character symbol representing the \"current\" language. e.g. en"
  (string->symbol
   (string-take
    (or (falsify-string (getenv "LC_MESSAGES"))
        (falsify-string (getenv "LC_ALL"))
        "en")
    2)))
