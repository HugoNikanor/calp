(define-module (text numbers sv)
  :use-module ((srfi srfi-1) :select (every))
  :use-module (srfi srfi-71)
  :use-module (hnh util)
  :export (number->string-cardinal
           number->string-ordinal
           each-string))

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

(define (number->string-cardinal n . _)
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
        [(= n 16) "sexton"]
        [(= n 17) "sjutton"]
        [(= n 18) "arton"]
        [(= n 19) "nitton"]
        [(= n 20) "tjugo"]
        [(<= 21 n 29) (string-append "tjugo" (number->string-cardinal
                                             (- n 20)))]
        [(= n 30) "trettio"]
        [(<= 31 n 39) (string-append "trettio" (number->string-cardinal
                                               (- n 30)))]
        [(= n 40) "fyrtio"]
        [(<= 31 n 49) (string-append "fyrtio" (number->string-cardinal
                                              (- n 40)))]
        [(<= 50 n 69) (let ((big small (floor/ n 10)))
                       (string-append
                        (number->string-cardinal big)
                        "tio"
                        (if (zero? small)
                            "" (number->string-cardinal small))))]
        [(= n 70) "sjuttio"]
        [(<= 71 n 79) (string-append "sjuttio" (number->string-cardinal (- n 70)))]
        [(= n 80) "åttio"]
        [(<= 81 n 89) (string-append "åttio" (number->string-cardinal (- n 80)))]
        [(= n 90) "nittio"]
        [(<= 91 n 99) (string-append "nittio" (number->string-cardinal (- n 90)))]
        [(= n 100) "etthundra"]
        [(< 100 n 200) (string-append "etthundra" (number->string-cardinal (- n 100)))]
        [(= n 200) "tvåhundra"]
        [(< 200 n 1000) (let ((big small (floor/ n 100)))
                          (string-append
                           (number->string-cardinal big)
                           "hundra"
                           (if (zero? small)
                               "" (number->string-cardinal small))))]

        [(<= 1000 n 999999)
         (let ((big small (floor/ n 1000)))
           (let ((big* (number->string-cardinal big)))
             (string-append
              (if (= 1 big) "et" big*)
              (if (<= 18 (string-length big*)) " " "")
              "tusen"
              (if (zero? small)
                  ""
                  (string-append
                   " " (number->string-cardinal small))))))]

        [(<= #e1e6 n (1- #e1e66))
         (let ((e (inexact->exact (floor (log10 n))))
               (big small (floor/ n #e1e6)))
           (if (zero? big)
               (number->string-cardinal (modulo small 1000))
               (string-append
                (if (or (= 1 big)
                        (= 0 (modulo big 1000)))
                    "en"
                    (number->string-cardinal
                     (floor-quotient
                      big (if (even? (floor-quotient e 3))
                              1 1000))))
                " "
                (large-prefix e)
                (if (even? (floor-quotient e 3))
                    "iljon" "iljard")
                (if (or (= 1 big)
                        (= 0 (modulo big 1000)))
                    "" "er")
                (if (zero? small)
                    ""
                    (string-append
                     " "
                     (number->string-cardinal small))))))]
        [else
         ;; I give up, don't have larger numbers that that!
         (string-append "det stora talet "
                        (number->string n))]))

(define* (number->string-ordinal
                 n key: a-form? allow-other-keys:)
  (define a-string (if a-form? "a" "e"))
  (cond [(>= -3 n) (string-append (number->string-ordinal (- n)) " sista" )]
        [(= -2 n) "näst sista"]
        [(= -1 n) "sista"]
        [(= 0 n) "nollte"]              ;
        [(= 1 n) (string-append "först" a-string)]
        [(= 2 n) (string-append "andr" a-string)]
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
        [(= 19 n) "nittonde"]
        [(= 20 n) "tjugonde"]
        [(<= 20 n 29) (string-append "tjugo"
                                    (number->string-ordinal
                                     (- n 20)
                                     a-form?: a-form?))]

        [(<= 30 n 99)
         (let ((big small (floor/ n 10)))
           (string-append
            (case big
              [(3) "tret"]
              [(4) "fyr"]
              [(8) "åt"]
              [(7) "sjut"]
              [(9) "nit"]
              [else (number->string-cardinal big)])
            "tio"
            (if (zero? (modulo small 10))
                "nde"
                (number->string-ordinal
                 small a-form?: a-form?))))]
        [(= n 100) "etthundrade"]
        [(= n 1000) "etttusende"]
        [(= n #e1e6) "miljonte"]
        [(= n #e1e9) "miljarde"]
        [(= n #e1e12) "biljonte"]

        [else
         (let ((big small (floor/ n 100)))
           (string-append (number->string-cardinal (* big 100))
                          (if (zero? small)
                              "de"
                              (number->string-ordinal
                               small a-form?: a-form?))))]))

;; (each-string 1) ; => "varje"
;; (each-string 2) ; => "varannan"
;; (each-string 3) ; => "var tredje"
;; (each-string 3 #t) ; => "vart tredje"
(define* (each-string count optional: neutrum)
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
