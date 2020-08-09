;; TODO rename this module
(define-module (output html-search)
  :use-module (util)
  :use-module (vcomponent)
  :use-module (vcomponent search)
  :use-module (ice-9 format)
  )

(define-public (search-result-page search-term search-result page paginator q=)
  `(*TOP*
    (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
    (html (@ (xmlns "http://www.w3.org/1999/xhtml") (lang sv))
          (head (title "Search results")
                ;; TODO (@ (output html) include-css)
                (link (@ (type "text/css")
                         (rel "stylesheet")
                         (href "/static/style.css"))))
          (body
           (h2 "Search term")
           (form
            (pre (textarea (@ (name "q") (rows 5) (spellcheck false)
                              (style "width:100%"))
                           ,(format #f "~y" search-term)))
            (input (@ (type submit))))
           (h2 "Result (page " ,page ")")
           (ul
            ,@(for event in search-result
                   `(li (@ (class "event"))
                        ,(prop event 'SUMMARY))))
           (div (@ (class "paginator"))
                ,@(paginator->list
                   paginator
                   (lambda (p) (if (= p page)
                              `(span ,p)
                              `(a (@ (href "?" ,q= "&p=" ,p)) ,p)))
                   (lambda (p) `(a (@ (href "?" ,q= "&p=" ,p)) "»"))))
           ))))

