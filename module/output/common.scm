;;; Commentary:
;; Common code between all serializers.
;; (ical and xcal). Not for graphical output stuff.
;;; Code:

(define-module (output common)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (vcomponent)
  )

;; TODO this is also defined @ (vcomponent parse component)
(define-public (x-property? symb)
  (string=? "X-" (string-take (symbol->string symb) 2)))

;; TODO this is also defined twice
(define-public (generate-uuid)
  ((@ (rnrs io ports) call-with-port)
   ((@ (ice-9 popen) open-input-pipe) "uuidgen")
   (@ (ice-9 rdelim) read-line)))

(define*-public (internal-field? symbol optional: (prefix "X-HNH-"))
  (string=? prefix
            (string-take-to (symbol->string symbol)
                            (string-length prefix))))

(define-public (->string a)
  (with-output-to-string (lambda () (display a))))

(define-public (get-tz-names events)
  (lset-difference
   equal? (lset-union
           equal? '("dummy")
           (filter-map
            (lambda (vline) (and=> (prop vline 'TZID) car))
            (filter-map (extract* 'DTSTART)
                        events)))
   '("dummy" "local")))

(define-public (downcase-symbol symb)
  (-> symb
      symbol->string
      string-downcase
      string->symbol))
