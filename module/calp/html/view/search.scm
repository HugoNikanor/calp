(define-module (calp html view search)
  :use-module (hnh util)
  :use-module (vcomponent)
  :use-module (vcomponent util search)
  :use-module ((ice-9 pretty-print) :select (pretty-print))
  :use-module ((web uri-query) :select (encode-query-parameters))
  :use-module ((calp html components)
               :select (xhtml-doc include-css))
  :use-module ((calp html vcomponent)
               :select (compact-event-list))
  )

;; Display the result of a search term, but doesn't do any searching
;; on its own.
;; 
;; @var{errors} : #f or SXML object to display instead of search result
;; @var{has-query?} : Does search-term actually contain anything, or should
;;         it be handled as a blank query?
;; @var{search-term} : What was searched, as an SEXP
;; @var{search-result} : The list of matched events
;; @var{page} : Which page we are on
;; @var{paginator} : A paginator object
(define-public (search-result-page
                errors has-query? search-term search-result page paginator)
  (xhtml-doc
   (@ (lang sv))
   (head (title "Search results")
         ,(include-css "/static/style.css"))
   (body
    (a (@ (href ("/today"))) "Till Idag")
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
                               `(a (@ (href
                                       "?"
                                       ,(encode-query-parameters
                                         `((p . ,p)
                                           (q . ,search-term)))))
                                   ,p)))
                    (lambda (p) `(a (@ (href
                                   "?"
                                   ,(encode-query-parameters
                                     `((p . ,p)
                                       (q . ,search-term)))))
                               "»")))))))))
