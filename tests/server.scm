;;; Commentary:
;; Tests parse-endpoint-string, used for defining server routes.
;;; Code:

(test-assert (parse-endpoint-string "/static/:dir/:file"))
;; => "/static/([^/]+)/([^/]+)"
;; => (dir file)

