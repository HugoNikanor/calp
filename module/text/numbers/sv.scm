(define-module (text numbers sv)
  :use-module ((srfi srfi-1) :select (every))
  :use-module (srfi srfi-71)
  :use-module (hnh util)
  :use-module (hnh util type)
  :export (number->string-cardinal
           number->string-ordinal
           each-string))

(define (exp-name x)
  (case x
    ((0)  "")
    ((1)  "tusen")
    ((2)  " miljon")
    ((3)  " miljard")
    ((4)  " biljon")
    ((5)  " biljard")
    ((6)  " triljon")
    ((7)  " triljard")
    ((8)  " kvadriljon")
    ((9)  " kvadriljard")
    ((10) " kvintiljon")
    ((11) " kvintiljard")
    ((12) " sextiljon")
    ((13) " sextiljard")
    ((14) " septiljon")
    ((15) " septiljard")
    ((16) " oktiljon")
    ((17) " oktiljard")
    ((18) " noniljon")
    ((19) " noniljard")
    ((20) " deciljon")
    ((21) " deciljard")))

(define* (number->string-cardinal n key: (e3 0) allow-other-keys:)
  (cond [(< n 0) (string-append "minus " (number->string-cardinal (- n)))]
        [else
         (let loop ((n n)          ; Remaining part of number
                    (e3 e3)         ; tenth exporent, divided by three
                    (have-larger #f)  ; Is there a larger part of this number
                    )
           (cond
            [(= n 0) "noll"]
            [(and (= n 1) (= e3 0)) "ett"]
            [(and (= n 1) (= e3 1)) "ettusen"]
            [(= n 1)  (string-append "en"      (exp-name e3))]
            [(= n 2)  (string-append "två"     (exp-name e3))]
            [(= n 3)  (string-append "tre"     (exp-name e3))]
            [(= n 4)  (string-append "fyra"    (exp-name e3))]
            [(= n 5)  (string-append "fem"     (exp-name e3))]
            [(= n 6)  (string-append "sex"     (exp-name e3))]
            [(= n 7)  (string-append "sju"     (exp-name e3))]
            [(= n 8)  (string-append "åtta"    (exp-name e3))]
            [(= n 9)  (string-append "nio"     (exp-name e3))]
            [(= n 10) (string-append "tio"     (exp-name e3))]
            [(= n 11) (string-append "elva"    (exp-name e3))]
            [(= n 12) (string-append "tolv"    (exp-name e3))]
            [(= n 13) (string-append "tretton" (exp-name e3))]
            [(= n 14) (string-append "fjorton" (exp-name e3))]
            [(= n 15) (string-append "femton"  (exp-name e3))]
            [(= n 16) (string-append "sexton"  (exp-name e3))]
            [(= n 17) (string-append "sjutton" (exp-name e3))]
            [(= n 18) (string-append "arton"   (exp-name e3))]
            [(= n 19) (string-append "nitton"  (exp-name e3))]

            [(<= 20 n 99)
             (let ((tens ones (floor/ n 10)))
               (string-append
                (case tens
                  ((2) "tjugo")
                  ((3) "trettio")
                  ((4) "fyrtio")
                  ((5) "femtio")
                  ((6) "sextio")
                  ((7) "sjuttio")
                  ((8) "åttio")
                  ((9) "nittio"))
                (if (zero? ones)
                    "" (number->string-cardinal ones))
                (exp-name e3)))]

            [(<= 100 n 999)
             (let ((big small (floor/ n 100)))
               (string-append
                (number->string-cardinal big)
                "hundra"
                (if (zero? small)
                    ""
                    (number->string-cardinal small))
                (if (and have-larger (= e3 1)) " " "")
                (exp-name e3)))]

            [(< n (1- #e1e66))
             (let ((head tail (floor/ n 1000)))
               (string-append
                (loop head (+ e3 1) #f)
                (cond ((= 1 head) "")
                      ((= 0 (floor-remainder head 1000)) "")
                      ((>= e3 1) "er")
                      (else ""))
                (cond ((= 0 tail) "")
                      (else (string-append
                             " " (loop tail e3 #t))))))]


            [else
             ;; I give up, don't have larger numbers that that!
             (string-append "det stora talet "
                            (number->string n))]))]))

(define* (number->string-ordinal
                 n key: a-form? allow-other-keys:)
  (define a-string (if a-form? "a" "e"))

  (define (main-part n e)
    (case e
      ((0) (cond [(>= -3 n) (string-append (number->string-ordinal (- n) a-form?: a-form?) " sista" )]
                 [(= -2 n) "näst sista"]
                 [(= -1 n) "sista"]
                 [(= 0 n)  "nollte"]      ;
                 [(= 1 n)  (string-append "först" a-string)]
                 [(= 2 n)  (string-append "andr" a-string)]
                 [(= 3 n)  "tredje"]
                 [(= 4 n)  "fjärde"]
                 [(= 5 n)  "femte"]
                 [(= 6 n)  "sjätte"]
                 [(= 7 n)  "sjunde"]
                 [(= 8 n)  "åttonde"]
                 [(= 9 n)  "nionde"]
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
                     (number->string-ordinal
                      small a-form?: a-form?)))]

                 [(= n 100) "etthundrade"]

                 [else
                  (let ((big small (floor/ n 100)))
                    (string-append (number->string-cardinal (* big 100))
                                   (if (zero? small)
                                       "de"
                                       (number->string-ordinal
                                        small a-form?: a-form?))))]))

      ((1) (string-append (number->string-cardinal n) "tusende"))
      (else (string-append
             (if (= n 1) "" (string-append (number->string-cardinal n) " "))
             (case e
               ((2) "miljonte")
               ((3) "miljarde")
               ((4) "biljonte"))))))

  (if (>= n #e1e15)
      (string-append (number->string n) ":e")
      (let loop ((n n) (e 0))
        (if (> 1000 n)
            (main-part n e)
            (let ((a b (floor/ n 1000)))
              (cond ((zero? b)
                     (loop a (1+ e)))
                    (else
                     (string-append
                      (number->string-cardinal a e3: (1+ e))
                      " "
                      (number->string-ordinal b a-form?: a-form?)))))))))


;; (each-string 1) ; => "varje"
;; (each-string 2) ; => "varannan"
;; (each-string 3) ; => "var tredje"
;; (each-string 3 #t) ; => "vart tredje"
(define* (each-string count key: neutrum allow-other-keys:)
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
