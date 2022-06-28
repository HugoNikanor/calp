;;; Commentary:
;; Tests parse-endpoint-string, used for defining server routes.
;;; Code:

(define-module (test server)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((web http make-routes)
               :select (parse-endpoint-string)))

(test-assert "Check that parsing doesn't crash"
  (parse-endpoint-string "/static/:dir/:file"))

;; Checks that parsing produces correct results
(test-group
  "Simple parameters"
  (let ((path args (parse-endpoint-string "/static/:dir/:file")))
    (test-equal "Path" "/static/([^/.]+)/([^/.]+)" path)
    (test-equal "Parameters" '(dir file) args)))

;; Checks that parsing with custom regex works
;; along with literal periods.
(test-group
  "Custom regex for parameters"
  (let ((path args (parse-endpoint-string "/static/:filename{.*}.:ext")))
    (test-equal "Path" "/static/(.*)\\.([^/.]+)" path)
    (test-equal "Parameters" '(filename ext) args)))
