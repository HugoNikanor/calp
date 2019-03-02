(define-module (vcalendar datetime)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)

  #:export (parse-datetime)
  )

(define (parse-datetime dtime)
  "Parse the given date[time] string into a date object."
  (localize-date
   (string->date
    dtime
    (case (string-length dtime)
      ((8) "~Y~m~d")
      ((15) "~Y~m~dT~H~M~S")
      ((16) "~Y~m~dT~H~M~S~z")))))
