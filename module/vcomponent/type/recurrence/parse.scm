(define-module (vcomponent type recurrence parse)
  ;; :duplicates (last)                   ; Replace @var{count}

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (datetime)
  :use-module (srfi srfi-26)
  :use-module (vcomponent type recurrence internal)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
  :use-module (ice-9 match)

  :export (rfc->datetime-weekday parse-day-spec))

;; transform into weekday objects from
(define (rfc->datetime-weekday symbol)
  (case symbol
    [(SU) sun]
    [(MO) mon]
    [(TU) tue]
    [(WE) wed]
    [(TH) thu]
    [(FR) fri]
    [(SA) sat]
    [else => (lambda (d)
               (scm-error 'misc-error "rfc->datetime-weekday"
                          "No such day ~a (~s)"
                          (list d (symbol->string d))
                          #f))]))

;; @example
;; <weekday> ∈ weekdays
;; <weekdaynum> ::= [[±] <num>] <weekday> ;; +3MO
;; (<weekadynum>, ...)
;; @end example

;;; weekdaynum can contain ±
;;; only used in bywdaylist
;;; only present with by BYDAY

;; Returns a pair, where the @code{car} is the offset
;; and @code{cdr} is the day symbol.
;; The @code{car} may be @code{#f}.
;; str → (<num> . <symb>)
(define (parse-day-spec str)
  (let* ((numerical-characters (append '(#\+ #\-) (map integer->char (iota 10 #x30))))
         (numbers letters (span (cut memv <> numerical-characters)
                                (string->list str))))
    (cons (string->number (list->string numbers))
          (rfc->datetime-weekday (apply symbol letters)))))


