;; TODO rename this module
(define-module (output html-search)
  :use-module (util)
  :use-module (vcomponent)
  :use-module (vcomponent search)
  :use-module ((ice-9 pretty-print) :select (pretty-print))
  :use-module (html components)
  :use-module (html vcomponent)
  )

(define-public (search-result-page
                search-term search-result page paginator q=)
  (xhtml-doc
   (@ (lang sv))
   (head (title "Search results")
         ,(include-css "/static/style.css"))
   (body
    (h2 "Search term")
    (form
     (pre (textarea (@ (name "q") (rows 5) (spellcheck false)
                       (style "width:100%"))
                    ,(with-output-to-string
                       (lambda () (pretty-print search-term)))))
     (input (@ (type submit))))
    (h2 "Result (page " ,page ")")
    (ul
     ,@(compact-event-list search-result))
    (div (@ (class "paginator"))
         ,@(paginator->list
            paginator
            (lambda (p) (if (= p page)
                       `(span ,p)
                       `(a (@ (href "?" ,q= "&p=" ,p)) ,p)))
            (lambda (p) `(a (@ (href "?" ,q= "&p=" ,p)) "»"))))
    )))

