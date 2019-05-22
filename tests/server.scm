(use-modules (server macro))

(parse-endpoint-string "/static/:dir/:file")
;; => "/static/([^/]+)/([^/]+)"
;; => (dir file)

