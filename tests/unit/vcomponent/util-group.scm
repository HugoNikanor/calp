(define-module (test vcomponent-util-group)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module ((vcomponent) :select (vcomponent-diff))
  :use-module (vcomponent create)
  :use-module (vcomponent util group))

(define (force-groups groups)
  (stream-map (lambda (p) (cons (car p) (stream->list (cdr p))))
              groups))

(define (test-groups expected actual)
  (for-each (lambda (group-e group-a)
              (test-equal "Matching dates" (car group-e) (car group-a))
              (test-equal "Same number of entries"
                (length (cdr group-e)) (length (cdr group-a)))
              (for-each (lambda (e a) (test-equal "Matching components"
                                   '() (vcomponent-diff e a)))
                        (cdr group-e) (cdr group-a)))
            expected actual))

(test-group "group-stream"
  (test-groups
   (list (list (date year: 2020 month: jan day: 1)
               (vevent
                dtstart: (datetime year: 2020 month: jan day: 1 hour: 0)
                dtend:   (datetime year: 2020 month: jan day: 1 hour: 10)))
         (list (date year: 2020 month: jan day: 02))
         (list (date year: 2020 month: jan day: 03))
         (list (date year: 2020 month: jan day: 04))
         (list (date year: 2020 month: jan day: 05))
         (list (date year: 2020 month: jan day: 06))
         (list (date year: 2020 month: jan day: 07))
         (list (date year: 2020 month: jan day: 08))
         (list (date year: 2020 month: jan day: 09))
         (list (date year: 2020 month: jan day: 10)
               (vevent
                dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
                dtend:   (datetime year: 2020 month: jan day: 10 hour: 10))
               (vevent
                dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
                dtend:   (datetime year: 2020 month: jan day: 14 hour: 10)))
         (list (date year: 2020 month: jan day: 11)
               (vevent
                dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
                dtend:   (datetime year: 2020 month: jan day: 14 hour: 10)))
         (list (date year: 2020 month: jan day: 12)
               (vevent
                dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
                dtend:   (datetime year: 2020 month: jan day: 14 hour: 10))
               (vevent
                dtstart: (date     year: 2020 month: jan day: 12)))
         (list (date year: 2020 month: jan day: 13)
               (vevent
                dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
                dtend:   (datetime year: 2020 month: jan day: 14 hour: 10))
               )
         (list (date year: 2020 month: jan day: 14)
               (vevent
                dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
                dtend:   (datetime year: 2020 month: jan day: 14 hour: 10))
               )
         (list (date year: 2020 month: jan day: 15)
               (vevent
                dtstart: (datetime year: 2020 month: jan day: 15 hour: 0)
                dtend:   (datetime year: 2020 month: jan day: 15 hour: 10)))
         (list (date year: 2020 month: jan day: 16)))

   (stream->list
    16 (force-groups
        (group-stream
         (stream (vevent dtstart: (datetime year: 2020 month: jan day: 1 hour: 0)
                         dtend:   (datetime year: 2020 month: jan day: 1 hour: 10))
                 (vevent dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
                         dtend:   (datetime year: 2020 month: jan day: 10 hour: 10))
                 (vevent dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
                         dtend:   (datetime year: 2020 month: jan day: 14 hour: 10))
                 (vevent dtstart: (date     year: 2020 month: jan day: 12))
                 (vevent dtstart: (datetime year: 2020 month: jan day: 15 hour: 0)
                         dtend:   (datetime year: 2020 month: jan day: 15 hour: 10))))))))


(test-group "get-groups-between"
  (let ((groups
         (group-stream
          (stream (vevent dtstart: (datetime year: 2020 month: jan day: 1 hour: 0)
                          dtend:   (datetime year: 2020 month: jan day: 1 hour: 10))
                  (vevent dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
                          dtend:   (datetime year: 2020 month: jan day: 10 hour: 10))
                  (vevent dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
                          dtend:   (datetime year: 2020 month: jan day: 14 hour: 10))
                  (vevent dtstart: (date     year: 2020 month: jan day: 12))
                  (vevent dtstart: (datetime year: 2020 month: jan day: 15 hour: 0)
                          dtend:   (datetime year: 2020 month: jan day: 15 hour: 10))))))
    (test-group "Subset of all events"
      (test-groups
       (list (list (date year: 2020 month: jan day: 05))
             (list (date year: 2020 month: jan day: 06))
             (list (date year: 2020 month: jan day: 07))
             (list (date year: 2020 month: jan day: 08))
             (list (date year: 2020 month: jan day: 09))
             (list (date year: 2020 month: jan day: 10)
                   (vevent
	            dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
	            dtend:   (datetime year: 2020 month: jan day: 10 hour: 10))

                   (vevent
	            dtstart: (datetime year: 2020 month: jan day: 10 hour: 0)
	            dtend:   (datetime year: 2020 month: jan day: 14 hour: 10))))

       (stream->list
        (force-groups (get-groups-between groups
                                          (date year: 2020 month: jan day: 5)
                                          (date year: 2020 month: jan day: 10))))))

    (test-equal "Subset not containing any events"
        (list (list (date year: 2020 month: feb day: 5))
              (list (date year: 2020 month: feb day: 6))
              (list (date year: 2020 month: feb day: 7))
              (list (date year: 2020 month: feb day: 8))
              (list (date year: 2020 month: feb day: 9)))
      (stream->list (force-groups
                     (get-groups-between groups
                                         (date year: 2020 month: feb day: 5)
                                         (date year: 2020 month: feb day: 9)))))

    (test-group "Subset overlapping start of events"
      (test-groups
       (list (list (date year: 2019 month: dec day: 31))
             (list (date year: 2020 month: jan day: 1)
                   (vevent dtend:   (datetime year: 2020 month: 1 day: 1 hour: 10)
                           dtstart: (datetime year: 2020 month: 1 day: 1 hour: 0)))
             (list (date year: 2020 month: jan day: 2))
             (list (date year: 2020 month: jan day: 3))
             (list (date year: 2020 month: jan day: 4))
             (list (date year: 2020 month: jan day: 5)))

       (stream->list (force-groups
                      (get-groups-between groups
                                          (date year: 2019 month: dec day: 31)
                                          (date year: 2020 month: jan day: 5))))))

    (test-group "Subset overlapping end of events"
      (test-groups
       (list (list (date year: 2020 month: jan day: 11)
                   (vevent dtend:   (datetime year: 2020 month: jan day: 14 hour: 10)
                           dtstart: (datetime year: 2020 month: jan day: 10 hour: 00)))
             (list (date year: 2020 month: jan day: 12)
                   (vevent dtend:   (datetime year: 2020 month: jan day: 14 hour: 10)
                           dtstart: (datetime year: 2020 month: jan day: 10 hour: 00))
                   (vevent dtstart: (date year: 2020 month: jan day: 12)))
             (list (date year: 2020 month: jan day: 13)
                   (vevent dtend:   (datetime year: 2020 month: jan day: 14 hour: 10)
                           dtstart: (datetime year: 2020 month: jan day: 10 hour: 00)))
             (list (date year: 2020 month: jan day: 14)
                   (vevent dtend:   (datetime year: 2020 month: jan day: 14 hour: 10)
                           dtstart: (datetime year: 2020 month: jan day: 10 hour: 00)))
             (list (date year: 2020 month: jan day: 15)
                   (vevent dtend:   (datetime year: 2020 month: jan day: 15 hour: 10)
                           dtstart: (datetime year: 2020 month: jan day: 15 hour: 00)))
             (list (date year: 2020 month: jan day: 16))
             (list (date year: 2020 month: jan day: 17))
             (list (date year: 2020 month: jan day: 18))
             (list (date year: 2020 month: jan day: 19))
             (list (date year: 2020 month: jan day: 20)))

       (stream->list (force-groups
                      (get-groups-between groups
                                          (date year: 2020 month: jan day: 11)
                                          (date year: 2020 month: jan day: 20))))))))

;;; TODO group->event-list

'((vcomponent util group))
