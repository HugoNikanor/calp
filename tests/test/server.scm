;;; Commentary:
;; Tests parse-endpoint-string, used for defining server routes.
;;; Code:

(define-module (test server)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((web http make-routes)
               :select (parse-endpoint-string))
  :use-module ((hnh util) :select (let*)))

(test-assert "Check that parsing doesn't crash"
  (parse-endpoint-string "/static/:dir/:file"))

;; Checks that parsing produces correct results
(let* ((path args (parse-endpoint-string "/static/:dir/:file")))
  (test-equal "/static/([^/.]+)/([^/.]+)" path)
  (test-equal '(dir file) args))

;; Checks that parsing with custom regex works
;; along with literal periods.
(let* ((path args (parse-endpoint-string "/static/:filename{.*}.:ext")))
  (test-equal "/static/(.*)\\.([^/.]+)" path)
  (test-equal '(filename ext) args))
