(define-module (calp translation)
  :use-module (ice-9 i18n)
  :use-module (ice-9 regex)
  :use-module (ice-9 match)
  :export (_ yes-no-check))

(bindtextdomain "calp" "/home/hugo/code/calp/localization/")

(define (_ . msg)
  ;; NOTE this doesn't squeese repeated whitespace
  (string-map (match-lambda
                (#\newline #\space)
                (c c))
              (gettext (string-join msg) "calp")))

(define* (yes-no-check string #:optional (locale %global-locale))
  (cond ((string-match (locale-yes-regexp locale) string) 'yes)
        ((string-match (locale-no-regexp  locale) string) 'no)
        (else #f)))

