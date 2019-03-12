(define-module (terminal util)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-60)
  #:export (line ctrl color-escape))

(define* (line #:optional (width 64))
  (display (make-string width #\_))
  (newline))

(define (ctrl char)
  (integer->char (bitwise-and #b00011111 (char->integer char))))

(define-public (display-calendar-header! date)
  (let* ((day   (number->string (date-day   date)))
         (month (number->string (date-month date)))
         (year  (number->string (date-year  date))))
    (system* "cal" "-3" day month year)))


(define (color-escape n)
  (cond ((not n) "")
        ((char=? #\# (string-ref n 0))
         (let* ((str (string-drop n 1))
                (rs (substring str 0 2))
                (gs (substring str 2 4))
                (bs (substring str 4 6)))
           (format #f "\x1b[38;2;~a;~a;~am"
                   (string->number rs 16)
                   (string->number gs 16)
                   (string->number bs 16))))))
