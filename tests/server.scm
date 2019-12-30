(((server macro) parse-endpoint-string))

(test-assert (parse-endpoint-string "/static/:dir/:file"))
;; => "/static/([^/]+)/([^/]+)"
;; => (dir file)

