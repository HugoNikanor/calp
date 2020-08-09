;; TODO rename this module
(define-module (output html-search)
  :use-module (util)
  :use-module (vcomponent)
  :use-module (ice-9 format)
  )

(define-public (search-result-page search-term search-result mp q=)
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
           (h2 "Result")
           (ul
            ,@(for event in search-result
                   `(li (@ (class "event"))
                        ,(prop event 'SUMMARY))))
           (div (@ (class "paginator"))
                ,@(let ()
                    (define (make-link n) `(a (@ (href "?" ,q= "&p=" ,n))
                                              ,n))
                    (if (car mp)         ; true max page
                        (map make-link (iota (cdr mp)))
                        (append (map make-link (iota (cdr mp)))
                                `((a (@ (href "?" ,q= "&p=" ,(cdr mp)))
                                     "»"))))
                    ))
           ))))
