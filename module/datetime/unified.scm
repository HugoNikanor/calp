(define-module (datetime unified)
  :use-module (srfi srfi-1)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (datetime core)
  :use-module (datetime timezone)
  :use-module (datetime arithmetic)
  :export (datetime-difference
           datetime+ datetime-
           datetime= datetime=?
           datetime< datetime<? datetime<= datetime<=?
           datetime> datetime>? datetime>= datetime>=?
           datetime-min datetime-max
           timespan-overlaps?
           ))



(define (alternatives zoned naive x xs)
  (apply
   (if (zoned-datetime? x)
       zoned naive)
   x xs))



(define (datetime+ x . xs)
  (fold (swap (if (zoned-datetime? x) datetime+/zoneinfo datetime+/naive))
        x xs))

(define (datetime- x . xs)
  (fold (swap (if (zoned-datetime? x) datetime-/zoneinfo datetime-/naive))
        x xs))

(define (datetime-difference a b)
  ((if (zoned-datetime? a)
       datetime-difference/zoneinfo
       datetime-difference/naive)
   a b))

(define datetime=
  (case-lambda (() #t) ((x . xs) (alternatives datetime=/zoneinfo datetime=/naive x xs))))
(define datetime<
  (case-lambda (() #t) ((x . xs) (alternatives datetime</zoneinfo datetime</naive x xs))))
(define datetime<=
  (case-lambda (() #t) ((x . xs) (alternatives datetime<=/zoneinfo datetime<=/naive x xs))))
(define datetime>
  (case-lambda (() #t) ((x . xs) (alternatives datetime>/zoneinfo datetime>/naive x xs))))
(define datetime>=
  (case-lambda (() #t) ((x . xs) (alternatives datetime>=/zoneinfo datetime>=/naive x xs))))

(define datetime=?  datetime=)
(define datetime<?  datetime<)
(define datetime>?  datetime>)
(define datetime<=? datetime<=)
(define datetime>=? datetime>=)

(define (datetime-min a b)
  (if (datetime< a b) a b))

(define (datetime-max a b)
  (if (datetime< a b) b a))

;; @verbatim
;;    A          B          C          D          E         ¬F
;; |s1|     :     |s2| : |s1|     :     |s2| :          : |s1|
;; |  |     :     |  | : |  ||s2| : |s1||  | : |s1||s2| : |  |
;; |  ||s2| : |s1||  | : |  ||  | : |  ||  | : |  ||  | :
;;     |  | : |  |     : |  ||  | : |  ||  | : |  ||  | :     |s2|
;;     |  | : |  |     : |  |     :     |  | :          :     |  |
;;
;; Infinitely short ---+|s2| : |s1|+--- : two instants don't overlap
;; events, overlap   s1      :      s2  :
;; @end verbatim
;; 
;; E is covered by both case A and B.
(define (timespan-overlaps? s1-begin s1-end s2-begin s2-end)
  "Return whetever or not two timespans overlap."
  (typecheck s1-begin datetime?)
  (typecheck s1-end   datetime?)
  (typecheck s2-begin datetime?)
  (typecheck s2-end   datetime?)

  ;; TODO isn't this overly complicated?
  ;; Can't we just check if s1-begin is in [s2-begin, s2-end) or
  ;; s1-end is in [s2-begin, s2-end)?

  (or
   ;; A
   (and (datetime< s2-begin s1-end)
        (datetime< s1-begin s2-end))

   ;; B
   (and (datetime< s1-begin s2-end)
        (datetime< s2-begin s1-end))

   ;; C
   (and (datetime<= s1-begin s2-begin)
        (datetime< s2-end s1-end))

   ;; D
   (and (datetime<= s2-begin s1-begin)
        (datetime< s1-end s2-end))))
