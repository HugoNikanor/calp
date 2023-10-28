(define-module (test util-coverage)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (hnh util coverage))


;;; coverage-info
;;; coverage-info?
;;; filename lines total-lines hit-lines
;;; output-coverage
;;; parse-coverage
;;; merge-coverage

(test-equal
    (list
     (coverage-info
      filename: "filename"
      lines: '((2 . 3)
               (1 . 2))
      total-lines: 3
      hit-lines: 2))
  (parse-coverage
   "TN:
SF:filename
DA:1,2
DA:2,3
LH: 2
LF: 3
end_of_record"))


'((hnh util coverage))
