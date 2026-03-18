(define-module (calp entry-points make-timeline)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (datetime zoneinfo)
  :use-module (hnh util)
  :use-module (hnh util destructure)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (hnh util options)
  :use-module (ice-9 control)
  :use-module (ice-9 getopt-long)
  :export (%summary main))

(define %summary "Script to generate diagrams of active timezone rules.")

;;; start, end ::= integer?
;;; label      ::= string?
;;; interval   ::= (list-of (tuple-of start end label))
;;; intervals  ::= (list-of interval)

;;; entry ::= (pair-of start label)

(define-type (interval-entry)
  ;; Both start and end inclusive
  (entry-start type: integer? keyword: start)
  (entry-end   type: integer? keyword: end)
  (entry-label type: string? keyword: label))

(define (entry->interval-entry entry)
  (interval-entry start: (car entry)
                  end: (1- (+ (car entry) (string-length (cdr entry))))
                  label: (cdr entry)))

(define (attempt-insert-interval entry interval)
  (define entry-start (car entry))
  (define entry-end (1- (+ entry-start (string-length (cdr entry)))))

  ;; - we start AFTER end of previous entry
  ;; - we end BEFORE start of current entry
  ;;   INSERT
  ;; - we start BEFORE end of current
  ;;   CONFLICT
  ;; - we start AFTER start of next entry
  ;;   we might have space in future

  (call/ec
   (lambda (return)
     (let loop ((last-end 0)
                (remaining interval))
       (if (<= entry-start last-end)
           (return #f)
           (destructure remaining
             ('() (list (entry->interval-entry entry)))
             ((cons (and (interval-entry start: s)
                         (< entry-end s))
                    _)
              (cons (entry->interval-entry entry)
                    remaining))
             ((cons (@ x (interval-entry end: e)) xs)
              (cons x (loop e xs)))))))))


(define (insert-intervals entry intervals)
  (let loop ((intervals intervals))
    (cond ((null? intervals)
           ;; No space in any interval, create a new one
           (list (list (entry->interval-entry entry))))
          ((attempt-insert-interval entry (car intervals))
           => (lambda (updated) (cons updated (cdr intervals))))
          (else
           ;; No space in this interval, try rest
           (cons (car intervals)
                 (loop (cdr intervals)))))))

(define* (get-intervals rule-name optional: (label-prefix ""))
 (fold (lambda (ixed-rule collected)
         (destructure ixed-rule
           ((cons ix (zi-rule rule-save: (cons type _) rule-to: to rule-from: from))
            (let ((ix (1+ ix)))
              (define label (string-append
                             label-prefix
                             (number->string ix)
                             (case type ((standard) "S") ((daylight) "D"))))
              (define label-width (string-length label))
              (define interval-label
                (case to
                  ((only) (format #f "║~a" label))
                  ((maximum) (format #f "├─~a─┄" label))
                  (else
                   (let ((len (1+ (- to from))))
                     (cond ((= len (+ 2 label-width))
                            (format #f "├~a┤" label))
                           ((< (+ 2 label-width) len)
                            (format #f "├─~a~a┤"
                                    label (make-string (- len label-width 1 2) #\─)))
                           (else (format #f "├~a┤~a"
                                         (make-string (- len 2) #\─)
                                         label)))))))

              (insert-intervals (cons from interval-label)
                                collected)))))
       '(())
       (enumerate (get-rule (zoneinfo) rule-name))))

(define interval-min (compose entry-start car))
(define interval-max (compose entry-end last))


(define (concat-interval interval global-start)
  (if (null? interval)
      ""
      (destructure (car interval)
        ((interval-entry start: start end: end label: label)
         (string-append (make-string (- start global-start) #\space)
                        label
                        (concat-interval (cdr interval)
                                         (1+ end)))))))

(define (draw-timeline start end)
 (define (draw-timeline-1 start end)
   (string-concatenate
    (map (lambda (i)
           (cond ((zero? (modulo i 10)) "┰")
                 ((even? i) "┬")
                 (else "─")))
         (iota (1+ (- end start)) start))))

 (define (draw-timeline-2 start end)
   (string-trim-right
    (string-concatenate
     (map (lambda (i) (if (zero? (modulo i 10)) "┃" " "))
          (iota (1+ (- end start)) start)))))

 (define (draw-timeline-3 start end)
   ;; leading whitespace
   (define fixed-start (* 10 (ceiling (/ start 10))))
   (string-trim-right
    (string-append
     (make-string (- fixed-start start) #\space)
     (string-concatenate
      (map (lambda (i)
             (let ((label (number->string i)))
               (string-append label (make-string (- 10 (string-length label)) #\space))))
           (iota (ceiling (/ (1+ (- end fixed-start)) 10)) fixed-start 10))))))

 (list (draw-timeline-1 start end)
       (draw-timeline-2 start end)
       (draw-timeline-3 start end)))


(define (draw-intervals intervals)
  (define start (apply min (map interval-min intervals)))
  (define end   (apply max (map interval-max intervals)))
  (append
   (map (lambda (interval) (concat-interval interval start))
        (reverse intervals))
   (draw-timeline start end)
   ;; TODO we show ALL instances of all rules, even those not relevant for the selected zone.
   ;; This should show which rule was in effect during which time for the zone.
   #;
   (list
    (let loop ((start start)
               (remaining (list (cons 'US 1920)
                                (cons 'NYC 1942)
                                (cons 'US 1946)
                                (cons 'NYC 1967)
                                (cons 'US end))))
      (if (null? remaining)
          " \x1b[m"
          (string-append (case (caar remaining)
                           ((US) "\x1b[41m")
                           ((NYC) "\x1b[44m"))
                         (make-string (- (cdar remaining) start) #\space)
                         (loop (cdar remaining)
                               (cdr remaining))))))))




(define opt-spec '())

(define (number->label* i)
  (let ((rest d (floor/ i 26)))
    (cons
     (integer->char (+ d (char->integer #\A)))
     (if (zero? rest)
         '()
         (number->label* rest)))))

(define (number->label i)
  (reverse-list->string (number->label* i)))

(define (output-zone zone)
  (newline)
  (format #t "===== ~a =====~%" zone)
  (display
   (string-join (draw-intervals
                 (->> (get-zone (zoneinfo) zone)
                      (map zone-entry-rule)
                      enumerate
                      (filter (compose symbol? cdr))
                      (apply lset-adjoin (lambda (a b) (eq? (cdr a) (cdr b))) '())
                      ;; lset-adjoin reversed the list, reverse it back
                      reverse
                      (append-map (lambda (rule) (get-intervals (cdr rule)
                                                           (number->label (car rule)))))))
                "\n" 'suffix))

  ;; TODO majorly improve this format:
  ;; - actually handle direct rules
  ;; - format offset as hours, minutes, and seconds
  ;; - format until
  ;; - align columns for real, instead of hoping that tabs do it
  (format #t "# ID\tRULE\tFORMAT\tOFFSET\tUNTIL~%")
  (for (i . entry) in (enumerate (get-zone (zoneinfo) zone))
       (destructure entry
         ((zone-entry stdoff: offset
                      rule: rule
                      format: fmt
                      until: until)
          (format #t "  ~a\t~a\t~a\t~a\t~a~%"
                  (number->label i)
                  rule fmt offset until)))))

(define (output-rule rule)
  (newline)
  (format #t "===== ~a =====~%" rule)
  (display
   (string-join (draw-intervals (get-intervals rule))
                "\n" 'suffix)))

(define (main args)
  ;; (define opts (getopt-long args (getopt-opt opt-spec)))
  (let loop ((args (cdr args)))
    (destructure args
      ('() 'done)
      ((cons* "--zone" zone-name rest)
       (output-zone zone-name)
       (loop rest))
      ((cons* "--rule" rule-name rest)
       (output-rule (string->symbol rule-name))
       (loop rest)))))
