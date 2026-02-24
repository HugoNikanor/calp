(define-module (vcomponent type duration)
  :use-module ((datetime core) :select (datetime))
  :use-module (datetime duration)
  :use-module (hnh util type)
  :use-module (srfi srfi-88)
  :re-export (duration
              duration?

              duration-sign   duration-sign*
              duration-year   duration-year*
              duration-month  duration-month*
              duration-day    duration-day*
              duration-hour   duration-hour*
              duration-minute duration-minute*
              duration-second duration-second*

              duration-week*
              duration-time*

              duration-negate
              duration-negative?
              duration-positive?

              string->duration
              duration->string)
  :export (
           duration->datetime
           valid-icalendar-duration?
           ))



;;; Checks if the given duration is valid for use in an iCalendar stream.
(define (valid-icalendar-duration? dur)
  (and (= 0 (duration-year dur) (duration-month dur))))


;;; DEPRECATED
(define (duration->datetime duration)
  (typecheck duration duration?)
  (values (duration-sign duration)
          (datetime year:   (duration-year   duration)
                    month:  (duration-month  duration)
                    day:    (duration-day    duration)
                    hour:   (duration-hour   duration)
                    minute: (duration-minute duration)
                    second: (duration-second duration))))
