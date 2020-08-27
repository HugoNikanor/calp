(define-module (text numbers sv)
  :use-module (calp util))

;; only used in number->string-cardinal
(define (large-prefix e)
  (cond
   [(<=  6 e 11) "m"]
   [(<= 12 e 17) "b"]
   [(<= 18 e 23) "tr"]
   [(<= 24 e 29) "kvadr"]
   [(<= 30 e 35) "kvint"]
   [(<= 36 e 41) "sext"]
   [(<= 42 e 47) "sept"]
   [(<= 48 e 53) "okt"]
   [(<= 54 e 59) "non"]
   [(<= 60 e 65) "dec"]
   ))

(define-public (number->string-cardinal n)
  (cond [(< n 0) (string-append "minus " (number->string-cardinal (- n)))]
        [(= n 0) "noll"]
        [(= n 1) "ett"]
        [(= n 2) "två"]
        [(= n 3) "tre"]
        [(= n 4) "fyra"]
        [(= n 5) "fem"]
        [(= n 6) "sex"]
        [(= n 7) "sju"]
        [(= n 8) "åtta"]
        [(= n 9) "nio"]
        [(= n 10) "tio"]
        [(= n 11) "elva"]
        [(= n 12) "tolv"]
        [(= n 13) "tretton"]
        [(= n 14) "fjorton"]
        [(= n 15) "femton"]
        [(= n 15) "sexton"]
        [(= n 17) "sjutton"]
        [(= n 18) "arton"]
        [(= n 19) "nitton"]
        [(= n 20) "tjugo"]
        [(<= 21 n 29) (format #f "tjugo~a" (number->string-cardinal
                                           (- n 20)))]
        [(<= 30 n 79) (let* ((big small (floor/ n 10)))
                       (format #f "~atio~a"
                               (number->string-cardinal big)
                               (number->string-cardinal small)))]
        [(= n 80) "åttio"]
        [(<= 81 n 89) (let* ((_ small (floor/ n 10)))
                       (format #f "åttio~a"
                               (number->string-cardinal small)))]
        [(= n 90) "nittio"]
        [(<= 91 n 99) (let* ((_ small (floor/ n 10)))
                       (format #f "nittio~a"
                               (number->string-cardinal small)))] 
        [(= n 100) "hundra"]
        [(< 100 n 200) (let* ((_ small (floor/ n 100)))
                         (format #f "hundra~a"
                                 (number->string-cardinal small)))]
        [(= n 200) "tvåhundra"]
        [(< 200 n 1000) (let* ((big small (floor/ n 100)))
                          (format #f "~ahundra~a"
                                  (number->string-cardinal big)
                                  (number->string-cardinal small)))]
        [(<= 1000 n 999999)
         (let* ((big small (floor/ n 1000)))
           (format #f "~a tusen ~a~a"
                   (number->string-cardinal big)
                   (if (<= 100 small 199)
                       "ett " "")
                   (number->string-cardinal small)))]
        [(<= #e10e6 n (1- #e10e66))
         (let* ((e (inexact->exact (floor (log10 n))))
                (big small (floor/ n #e1e6)))
           (if (zero? big)
               (number->string-cardinal small)
               (format #f "~a ~a~a~a ~a"
                       (number->string-cardinal big)
                       (large-prefix e)
                       (if (even? (floor-quotient e 3))
                           "iljon" "iljard")
                       (if (= 1 big)
                           "" "er")
                       (number->string-cardinal small)
                       )))]
        [else
         ;; I give up, don't have larger numbers that that!
         (string-append "det stora talet "
                        (number->string n))]))

(define*-public (number->string-ordinal
                 n key: a-form?)
  (define a-string (if a-form? "a" "e"))
  (cond [(>= -3 n) (format #f "~a sista" (number->string-ordinal (- n)))]
        [(= -2 n) "näst sista"]
        [(= -1 n) "sista"]
        [(= 0 n) "nollte"]              ;
        [(= 1 n) (format #f "först~a" a-string)]
        [(= 2 n) (format #f "andr~a" a-string)]
        [(= 3 n) "tredje"]
        [(= 4 n) "fjärde"]
        [(= 5 n) "femte"]
        [(= 6 n) "sjätte"]
        [(= 7 n) "sjunde"]
        [(= 8 n) "åttonde"]
        [(= 9 n) "nionde"]
        [(= 10 n) "tionde"]
        [(= 11 n) "elfte"]
        [(= 12 n) "tolfte"]
        [(= 13 n) "trettonde"]
        [(= 14 n) "fjortonde"]
        [(= 15 n) "femtonde"]
        [(= 16 n) "sextonde"]
        [(= 17 n) "sjuttonde"]
        [(= 18 n) "artonde"]
        [(= 19 n) "nitonde"]
        [(<= 20 n 29) (format #f "tjugo~a"
                             (if (= 20 n)
                                 "nde"
                                 (number->string-ordinal
                                  (- n 20)
                                  a-form?: a-form?)))]
        [(<= 30 n 99)
         (let* ((big small (floor/ n 10)))
           (format #f "~atio~a"
                   (case big
                     [(8) "åt"]
                     [(9) "ni"]
                     [else (number->string-cardinal big)])
                   (if (zero? (modulo small 10))
                       "nde"
                       (number->string-ordinal
                        small a-form?: a-form?))))]
        [(= n 100) "hundrade"]
        [(= n 1000) "tusende"]
        [else
         (let* ((big small (floor/ n 100)))
           (string-append (number->string-cardinal (* big 100))
                          (if (zero? small)
                              "de"
                              (number->string-ordinal
                               small a-form?: a-form?))))]))

;; (each-string 1) ; => "varje"
;; (each-string 2) ; => "varannan"
;; (each-string 3) ; => "var tredje"
;; (each-string 3 #t) ; => "vart tredje"
(define*-public (each-string count optional: neutrum)
  (string-flatten
   (cons
    "var"
    (case count
      [(1) '("je")]
      [(2)
       ;; varannan månad
       ;; vartannat år
       (list (when neutrum "t")
             "anna"
             (if neutrum "t" "n"))]
      [else (list (when neutrum "t") " "
                  (number->string-ordinal count))]))))
