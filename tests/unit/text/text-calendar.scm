(define-module (test text-calendar)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (text calendar))

;;; TODO these tests are locale-dependant

(test-equal "Start and end week are partial"
  '("    oktober 2023    "
    "må ti on to fr lö sö"
    "                   1"
    " 2  3  4  5  6  7  8"
    " 9 10 11 12 13 14 15"
    "16 17 18 19 20 21 22"
    "23 24 25 26 27 28 29"
    "30 31               ")
  (graphical-calendar (date year: 2023 month: oct) wkst: mon))

(test-equal "End week is full, start is partial"
  '("     april 2023     "
    "må ti on to fr lö sö"
    "                1  2"
    " 3  4  5  6  7  8  9"
    "10 11 12 13 14 15 16"
    "17 18 19 20 21 22 23"
    "24 25 26 27 28 29 30"
    "                    ")
  (graphical-calendar (date year: 2023 month: apr) wkst: mon))

(test-equal "Start week is full, end is partial"
  '("    oktober 2023    "
    "sö må ti on to fr lö"
    " 1  2  3  4  5  6  7"
    " 8  9 10 11 12 13 14"
    "15 16 17 18 19 20 21"
    "22 23 24 25 26 27 28"
    "29 30 31            "
    "                    ")
  (graphical-calendar (date year: 2023 month: oct) wkst: sun))

(test-equal "Exact lineup"
  '("   februari 1800    "
    "to fr lö sö må ti on"
    " 1  2  3  4  5  6  7"
    " 8  9 10 11 12 13 14"
    "15 16 17 18 19 20 21"
    "22 23 24 25 26 27 28"
    "                    "
    "                    ")
  (graphical-calendar (date year: 1800 month: feb) wkst: thu))

(test-equal "Exact lineup"
  '("   februari 1800    "
    "to fr lö sö må ti on"
    " 1  2  3  4  5  6  7"
    " 8  9 10 11 12 13 14"
    "15 16 17 \x1b[7m18\x1b[m 19 20 21"
    "22 23 24 25 26 27 28"
    "                    "
    "                    ")
  (graphical-calendar (date year: 1800 month: feb day: 18) wkst: thu))



;;; This also tests that month overflow into next year works
(test-equal "Displayed calendar + overflowing into next year"
 (string-append
  "   november 2023         december 2023          januari 2024    \n"
  "må ti on to fr lö sö  må ti on to fr lö sö  må ti on to fr lö sö\n"
  "       1  2  3  4  5               1  2  3   1  2  3  4  5  6  7\n"
  " 6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14\n"
  "13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21\n"
  "20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28\n"
  "27 28 29 30           25 26 27 28 29 30 31  29 30 31            \n"
  "                                                                \n")
 (with-output-to-string
   (lambda ()
    (cal-3 (date year: 2023 month: dec)))))


'((text calendar))
