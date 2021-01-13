;;; Commentary:
;; Tests parse-endpoint-string, used for defining server routes.
;;; Code:

(((web http make-routes) parse-endpoint-string)
 ((calp util) let*))

(test-assert "Check that parsing doesn't crash"
  (parse-endpoint-string "/static/:dir/:file"))

;; Checks that parsing produces correct results
(let* ((path args (parse-endpoint-string "/static/:dir/:file")))
  (test-equal "/static/([^/]+)/([^/]+)" path)
  (test-equal '(dir file) args))


;; Checks that parsing with custom regex works
;; along with literal periods.
(let* ((path args (parse-endpoint-string "/static/:filename{.*}.:ext")))
  (test-equal "/static/(.*)\\.([^/.]+)" path)
  (test-equal '(filename ext) args))
