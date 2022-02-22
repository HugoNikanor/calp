(define-module (calp html caltable)

  :use-module (hnh util)
  :use-module (calp html util)
  :use-module (datetime)
  :use-module (srfi srfi-41)
  )

;; Small calendar similar to the one below.
;; TODO highlight days depending on which events they contain
;; TODO run this standalone, for embedding in other websites.
;; @example
;; må ti on to fr lö sö
;;  1  2  3  4  5  6  7
;;  8  9 10 11 12 13 14
;; 15 16 17 18 19 20 21
;; 22 23 24 25 26 27 28
;; 29 30
;; @end example
;; 
;; start-date : <date>
;; end-date   : <date>
;; next-start : <date> → <date>
;; prev-start : <date> → <date>
(define*-public (cal-table key: start-date end-date next-start prev-start)

  (define (->link date)
    (date->string date "~Y-~m-~d.html"))

  ;; (<date> → sxml-attributes) → <date> → sxml
  (define (td attr)
    (lambda (date)
      `(a (@ ,@(attr date))
          ;; NOTE This time object is the correct place to show the existance
          ;; of an event on a given day in this small calendar. For example
          ;; making the text red for all holidays, or creating a yellow background
          ;; for events from a specific source.
          (time (@ (datetime ,(date->string date "~Y-~m-~d")))
                ;; TODO should this field be translated?
                ,(day date)))))

  (define month-start (start-of-month start-date))
  (define pre-start (start-of-week month-start))
  (define month-end (end-of-month start-date))
  (define post-end (end-of-week month-end))

  `(div (@ (class "small-calendar"))

        ;; Cell 0, 0. The letter v. for week number
        (div (@ (class "column-head row-head")) "v.")

        ;; top row, names of week days
        ,@(map (lambda (d) `(div (@ (class "column-head"))
                            ;; TODO this SHOULD be translated
                            ,(string-titlecase (week-day-name d 2))))
               (weekday-list))

        ;; left columun, week numbers
        ,@(map (lambda (v) `(div (@ (class "row-head")) ,v))
               ;; TODO translate this
               (map week-number
                    (stream->list
                     (stream-take-while (lambda (s) (date<= s post-end))
                                        (week-stream pre-start)))))

        ;; actual days

        ,@(map (td (lambda (date)
                     `((class "prev")
                       (href ,(->link
                               ;; (prev-start date)
                               (iterate
                                prev-start
                                (lambda (d) (date<= d date (next-start d)))
                                start-date))
                             "#" ,(date-link date)))))
               (date-range pre-start (remove-day start-date)))


        ,@(map (td (lambda (date) `((href "#" ,(date-link date)))))
               (date-range start-date end-date))


        ,@(map (td (lambda (date)
                     `((class "next")
                       (href ,(->link
                               ;; (next-start date)
                               (iterate
                                next-start
                                (lambda (d) (and (date<= d date)
                                            (date< date (next-start d))))
                                start-date)) "#" ,(date-link date)))))
               (date-range (add-day end-date) post-end))))
