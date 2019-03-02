(define-module (vcalendar recur)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)

  #:use-module (vcalendar)
  )

(define s "FREQ=WEEKLY;UNTIL=20191130")

(define (generate-kv-pairs str)
  (map (cut string-split <> #\=)
       (string-split str #\;)))

(define upstring->symbol (compose string->symbol string-upcase))

(define-syntax-rule (ensure key val test)
  ((key)
   (let ((v val))
     (if (test v)
         v
         (throw 'bad-value key val))
     )))

#;
(let ((key 'FREQ)
      (val-base 'HOURLY))
  (case key
    (FREQ (upstring->symbol val-base)
          (memv <> '(SECONDLY MINUTELY HOURLY DAILY
                           WEEKLY MONTHLY YEARLY)))))

(define-immutable-record-type <recur-rule>
  (make-recur-rules freq until count interval)
  recur-rule?
  (freq get-freq set-freq)
  (until get-until set-until)
  (count get-count set-count)
  (interval get-interval set-interval)))

(let ((s->n string->number))
 (reduce (lambda (kv rule)
        (let ((key (upstring->symbol (car kv)))
              (val-base (cadr kv)))
          (case key
            ((FREQ)
             (set-freq rule
                       (ensure (upstring->symbol val-base)
                               (cut memv <>
                                    '(SECONDLY MINUTELY HOURLY DAILY
                                               WEEKLY MONTHLY YEARLY)))))
            

            ((UNTIL)
             (set-until rule (parse-datetime val-base)))

            ((COUNT) (set-count rule (s->n val-base)))

            ((INTERVAL) (set-internal rule (s->n val-base)))

            ((BYSECOND) (let ((s (s->n val-base)))
                          (<= 0 s 60)))

            ((BYMINUTE) (let ((m (s->n val-base)))
                          (<= 0 m 59)))

            ((BYHOUR) (let ((h (s->n val-base)))
                        (<= 0 h 23)))
            
            #|
            ((BYDAY) #; TODO )

            ((BYMONTHDAY) #; TODO)
            ((BYYEARDAY)  )
            ((BYWEEKNO)  )
            ((BYMONTH)  )
            ((BYSETPOS) )
            ((WKST)  )
            |#
            (else 'err))))

      (generate-kv-pairs s)))



#|
Each recuring event should be expanded to a stream of all it's occurances.

The first instance of the event is at DTSTART,
times for following instances are calculating according to the DSL below. 
     
3.3.10. Recurrence Rule
     Value Name: RECUR
|#


#|
       byseclist   = ( seconds *("," seconds) )

       seconds     = 1*2DIGIT       ;0 to 60

       byminlist   = ( minutes *("," minutes) )

       minutes     = 1*2DIGIT       ;0 to 59

       byhrlist    = ( hour *("," hour) )

       hour        = 1*2DIGIT       ;0 to 23

       bywdaylist  = ( weekdaynum *("," weekdaynum) )

       weekdaynum  = [[±] ordwk] weekday

       ordwk       = 1*2DIGIT       ;1 to 53

       weekday     = "SU" / "MO" / "TU" / "WE" / "TH" / "FR" / "SA"
       ;Corresponding to SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY,
       ;FRIDAY, and SATURDAY days of the week.



Desruisseaux                Standards Track                    [Page 39]

RFC 5545                       iCalendar                  September 2009


       bymodaylist = ( monthdaynum *("," monthdaynum) )

       monthdaynum = [±] ordmoday

       ordmoday    = 1*2DIGIT       ;1 to 31

       byyrdaylist = ( yeardaynum *("," yeardaynum) )

       yeardaynum  = [±] ordyrday

       ordyrday    = 1*3DIGIT      ;1 to 366

       bywknolist  = ( weeknum *("," weeknum) )

       weeknum     = [±] ordwk

       bymolist    = ( monthnum *("," monthnum) )

       monthnum    = 1*2DIGIT       ;1 to 12

       bysplist    = ( setposday *("," setposday) )

       setposday   = yeardaynum



|#
