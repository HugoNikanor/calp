(define-module (calp html view search)
  :use-module (calp util)
  :use-module (vcomponent)
  :use-module (vcomponent search)
  :use-module ((ice-9 pretty-print) :select (pretty-print))
  :use-module ((calp html components)
               :select (xhtml-doc include-css))
  :use-module ((calp html vcomponent)
               :select (compact-event-list))
  )

(define-public (search-result-page
                errors
                has-query? search-term search-result page paginator q=)
  (xhtml-doc
   (@ (lang sv))
   (head (title "Search results")
         ,(include-css "/static/style.css"))
   (body
    (h2 "Search term")
    (form
     (pre (textarea (@ (name "q") (rows 5) (spellcheck false)
                       (style "width:100%"))
                    ,(when has-query?
                       (with-output-to-string
                         (lambda () (pretty-print search-term))))))
     (label (@ (for "onlyfuture")) "limit to future occurences")
     (input (@ (name "onlyfuture") (id "onlyfuture") (type checkbox)))
     (input (@ (type submit))))
    ,@(if errors
          `((h2 "Error searching")
            (div (@ (class "error"))
                 (pre ,errors)))
          `((h2 "Result (page " ,page ")")
            (ul ,@(compact-event-list search-result))
            (div (@ (class "paginator"))
                 ,@(paginator->list
                    paginator
                    (lambda (p) (if (= p page)
                               `(span ,p)
                               `(a (@ (href "?" ,q= "&p=" ,p)) ,p)))
                    (lambda (p) `(a (@ (href "?" ,q= "&p=" ,p)) "»")))))))))
