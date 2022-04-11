(define-module (calp translation)
  :use-module (ice-9 i18n)
  :use-module (ice-9 regex)
  :use-module (ice-9 match)
  :export (_ translate yes-no-check))

(bindtextdomain "calp" "/home/hugo/code/calp/localization/")


;; Translate string, but doesn't mark it for translation.
;; Used (at least) for get-config introspection procedures.
(define (translate string)
  ;; NOTE this doesn't squeese repeated whitespace
  (string-map (match-lambda
                (#\newline #\space)
                (c c))
              (gettext string "calp")))

;; Mark string for translation, and also make it discoverable for gettext
(define (_ . msg)
  (translate (string-join msg)))

(define* (yes-no-check string #:optional (locale %global-locale))
  (cond ((string-match (locale-yes-regexp locale) string) 'yes)
        ((string-match (locale-no-regexp  locale) string) 'no)
        (else #f)))

