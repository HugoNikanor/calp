(define-module (calp html view small-calendar)
  :use-module ((calp html components) :select (xhtml-doc include-css))
  :use-module ((calp html caltable) :select (cal-table))
  :use-module ((datetime) :select (month- month+ remove-day date->string))
  :export (render-small-calendar)
  )

(define (render-small-calendar month standalone)
  (define table (cal-table
                 start-date: month
                 end-date: (remove-day (month+ month))
                 next-start: month+
                 prev-start: month-
                 ))
  (if standalone
      (xhtml-doc
       (head (title ,(date->string month "~1"))
             ,(include-css "/static/smallcal.css"))
       (body ,table))
      table))
