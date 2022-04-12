(define-module (test translation)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :use-module (ice-9 i18n))

(define sv (make-locale (list LC_CTYPE LC_MESSAGES) "sv_SE.UTF-8"))

;; empty key should give us translation header
;; this also tests that translations are properly loaded
(test-assert "translations" (string? (translate "")))

(test-equal "yes-no yes" 'yes (yes-no-check "y" sv))
(test-equal "yes-no no" 'no (yes-no-check "n" sv))
(test-equal "yes-no invalid" #f (yes-no-check "other" sv))
