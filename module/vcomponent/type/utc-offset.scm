(define-module (vcomponent type utc-offset)
  :use-module (hnh util object)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :export (utc-offset
           utc-offset?
           utc-offset-value utc-offset-value*

           utc-offset->string
           ))

(define-type (utc-offset)
  (utc-offset-value type: exact-integer? keyword: value))

(define* (utc-offset->string offset key: (colon ":"))
  (string-append
   (if (negative? (utc-offset-value offset))
       "-" "+")
   (let ((t (seconds->time (abs (utc-offset-value offset)))))
     (time->string t
                   (string-join (if (zero? (second t))
                                    '("~H" "~M")
                                    '("~H" "~M" "~S"))
                                colon 'infix)))))
