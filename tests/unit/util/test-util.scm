(define-module (test test-util)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh test util))

(test-equal "µs" #e2e6 (µs 2))

(test-equal "transform-time-of-day"
  1698494486139313
  (transform-time-of-day
   ;; As returned by (gettimeofday)
   '(1698494486 . 139313)))

;; green
;; red
;; yellow
;; bold
;; make-indent

(test-equal "Indent"
  "        "
  (make-indent 4))

(test-equal "" (string-replace-head "" ""))
(test-equal "H" (string-replace-head "" "H"))
(test-equal "TAIL" (string-replace-head "TAIL" ""))
(test-equal "HAIL" (string-replace-head "TAIL" "H"))


;;; This is far from perfect
(test-assert "Is some form of diff produced"
  (string?
              (diff
               "This is the
first
string.
"
               "This is the
second
string.
")))


'((hnh test util))
