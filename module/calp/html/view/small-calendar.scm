(define-module (calp html view small-calendar)
  :use-module ((calp html components) :select (xhtml-doc include-css))
  :use-module ((calp html caltable) :select (cal-table))
  :use-module ((datetime) :select (date date+ date- date->string))
  :export (render-small-calendar)
  )

(define (render-small-calendar month standalone)
  (define table (cal-table
                 start-date: month
                 end-date: (date- (date+ month (date month: 1))
                                  (date day: 1))
                 next-start: (lambda (d) (date+ d (date day: 7)))
                 prev-start: (lambda (d) (date- d (date day: 7)))
                 ))
  (if standalone
      (xhtml-doc
       (head (title ,(date->string month "~1"))
             ,(include-css "/static/smallcal.css"))
       (body ,table))
      table))
