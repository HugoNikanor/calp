(define-module (test xmllint)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (hnh test xmllint))

;;; Really simple test, since all the logic is external.
;;; This just assures that xmllint is runnable on the current system, and that
;;; we handle input and output correctly.
(test-equal "xmllint"
  "<?xml version=\"1.0\"?>
<a href=\"#\">
  <b/>
</a>
"
  (xmllint "<a href=\"#\"><b/></a>"))


'((hnh test xmllint))
