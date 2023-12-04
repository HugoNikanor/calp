(define-module (text calendar)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (when unless group ->))
  :use-module ((hnh util io) :select (displayln))
  :use-module (hnh util type)
  :use-module (ice-9 format)
  :use-module ((texinfo string-utils) :select (center-string))
  :use-module (datetime)
  :use-module ((text util) :select (unwords))
  :export (graphical-calendar cal-3))

;; Done manually rather than through vulgar, so the text
;; utilities can become standalone.
(define (ansi-invert str)
  (format #f "\x1b[7m~a\x1b[m" str))

(define line-length (+ 6 (* 7 2)))
(define placeholder (gensym))

;;; Always returns 8 lines
;;; All lines are exactly 20 characters
(define* (graphical-calendar target-date
                             key:
                             (wkst mon)
                             (highlight-proc ansi-invert))
  (typecheck target-date date?)
  (typecheck target-date (or false? date?))

  ;; Date which starts the target month
  (define month-start (start-of-month target-date))

  ;; Number of days in the current way which is part of the previous month.
  (define to-skip (modulo (- (week-day (day target-date 1)) wkst) 7))

  (define lines ; (list-of (list-of (or integer? (eq? placeholder))))
    (let ((lines trail (floor/ (- (days-in-month target-date)
                                  (- 7 to-skip))
                               7)))
      (append
       (list (append (make-list to-skip placeholder) (iota (- 7 to-skip) 1)))
       (group (iota (* 7 lines) (1+ (- 7 to-skip))) 7)
       (list (append (iota trail (+ 1 (* 7 lines) (- 7 to-skip)) )
                     (make-list (- 7 trail) placeholder)))
       (when (= 3 lines)
         (list (make-list 7 placeholder))))))

  (append
   ;; Month and year declaration
   (list (center-string (date->string target-date "~B ~Y")
                        line-length))
   ;; Names of week days
   (list (unwords
          (map (lambda (d) (string-pad (week-day-name d 2) 2))
               (weekday-list wkst))))

   (map (lambda (group)
          (unwords
           (map (lambda (entry)
                  (cond ((and (integer? entry)
                              (= entry (day target-date)))
                         (highlight-proc (format #f "~2d" entry)))
                        ((integer? entry)
                         (format #f "~2d" entry))
                        (else "  ")))
                group)))
        lines)))


(define* (cal-3 optional: (d (current-date)))
  (typecheck d date?)
  ;; Day is set to one for month arithmetic,
  ;; and then to zero to not highlight any day
  (for-each displayln
            (map string-append
                 (graphical-calendar (-> d (day 1)
                                         (date- (date month: 1))
                                         (day 0)))
                 (make-list 8 "  ")
                 (graphical-calendar d)
                 (make-list 8 "  ")
                 (graphical-calendar (-> d (day 1)
                                         (date+ (date month: 1))
                                         (day 0))))))
