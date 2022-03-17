(define-module (base64)
  :use-module ((ice-9 optargs) :select (define*-public))
  :use-module ((srfi srfi-71) :select (let*))
  :use-module (srfi srfi-88) ; suffix keywords
  :use-module ((rnrs bytevectors)
               :select (bytevector-u8-ref
                        bytevector-u8-set!
                        bytevector-length
                        make-bytevector))
  :use-module ((rnrs io ports)
               :select (string->bytevector
                        bytevector->string
                        make-transcoder
                        latin-1-codec
                        native-transcoder)))

(define table
  (list->vector
   (map char->integer
        (string->list
         "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))))

(define (real->encoded byte)
  (vector-ref table byte))

(define (encoded->real byte)
  (define A (char->integer #\A))
  (define Z (char->integer #\Z))
  (define a (char->integer #\a))
  (define z (char->integer #\z))
  (define zero (char->integer #\0))
  (define nine (char->integer #\9))
  (cond [(= byte (char->integer #\=)) 0]
        [(= byte (char->integer #\+)) 62]
        [(= byte (char->integer #\/)) 63]
        [(<= A byte Z)
         (- byte A)]
        [(<= a byte z)
         (+ 26 (- byte a))]
        [(<= zero byte nine)
         (+ 26 26 (- byte zero))]
        [else (scm-error 'decoding-error
                         "encoded->real"
                         "Invalid character in Base64 string: ~s"
                         (list byte) #f)]))

(define ref
  (make-procedure-with-setter
   bytevector-u8-ref
   bytevector-u8-set!))

(define-public (base64->bytevector bv)
  (let ((len* (bytevector-length bv)))
    (if (zero? len*)
        (make-bytevector 0)
        (let* ((rest (+ (if (= (char->integer #\=) (ref bv (- len* 1)))
                            1 0)
                        (if (= (char->integer #\=) (ref bv (- len* 2)))
                            1 0)))
               (x (/ (- len* rest) 4))
               (ret-len (floor (* 3 x))))

          (define ret (make-bytevector ret-len))

          (do ((i 0 (1+ i)))
              ((>= i (floor x)))
            (let ((a (encoded->real (ref bv (+ (* i 4) 0))))
                  (b (encoded->real (ref bv (+ (* i 4) 1))))
                  (c (encoded->real (ref bv (+ (* i 4) 2))))
                  (d (encoded->real (ref bv (+ (* i 4) 3)))))
              (let ((aa (logior (ash a 2) (ash b -4)))
                    (ab (logior (ash (logand b #xF) 4) (ash c -2)))
                    (ac (logior (ash (logand c 3) 6) d)))
                (set! (ref ret (+ (* i 3) 0)) aa)
                (set! (ref ret (+ (* i 3) 1)) ab)
                (set! (ref ret (+ (* i 3) 2)) ac))))


          (case rest
            [(2)
             (let ((a (encoded->real (ref bv (+ (* (floor x) 4) 0))))
                   (b (encoded->real (ref bv (+ (* (floor x) 4) 1)))))
               (let ((aa (logior (ash a 2) (ash b -4))))
                 (set! (ref ret (- ret-len 1)) aa)))]
            [(1)
             (let ((a (encoded->real (ref bv (+ (* (floor x) 4) 0))))
                   (b (encoded->real (ref bv (+ (* (floor x) 4) 1))))
                   (c (encoded->real (ref bv (+ (* (floor x) 4) 2)))))
               (let ((aa (logior (ash a 2) (ash b -4)))
                     (ab (logior (ash (logand b #xF) 4) (ash c -2))))
                 (set! (ref ret (- ret-len 2)) aa)
                 (set! (ref ret (- ret-len 1)) ab)))])

          ret))))

(define-public (bytevector->base64 bv)
  (let* ((len (bytevector-length bv))
         (iterations rest (floor/ len 3)))
    (define ret (make-bytevector (+ (* 4 iterations)
                                    (if (zero? rest)
                                        0 4))))

    (do ((i 0 (1+ i)))
        ((>= i iterations))
      (let ((a (ref bv (+ (* i 3) 0)))
            (b (ref bv (+ (* i 3) 1)))
            (c (ref bv (+ (* i 3) 2))))
        (let ((aa (ash a -2))
              (ab (logior (ash (logand #b11 a) 4)   (ash b -4)))
              (ac (logior (ash (logand b #b1111) 2) (ash c -6)))
              (ad (logand c #x3F)))
          (set! (ref ret (+ (* i 4) 0)) (real->encoded aa))
          (set! (ref ret (+ (* i 4) 1)) (real->encoded ab))
          (set! (ref ret (+ (* i 4) 2)) (real->encoded ac))
          (set! (ref ret (+ (* i 4) 3)) (real->encoded ad)))))

    (case rest
      [(1)
       (let ((byte (ref bv (- len 1))))
         (let ((a (ash byte -2))
               (b (ash (logand byte 3) 4)))
           (set! (ref ret (+ 0 (* 4 iterations))) (real->encoded a))
           (set! (ref ret (+ 1 (* 4 iterations))) (real->encoded b))
           (set! (ref ret (+ 2 (* 4 iterations))) (char->integer #\=))
           (set! (ref ret (+ 3 (* 4 iterations))) (char->integer #\=))))]
      [(2)
       (let ((byte1 (ref bv (- len 2)))
             (byte2 (ref bv (- len 1))))
         (let ((a (ash byte1 -2))
               (b (logior (ash (logand byte1 3) 4) (ash byte2 -4 )))
               (c (ash (logand byte2 #xF) 2)))
           (set! (ref ret (+ 0 (* 4 iterations))) (real->encoded a))
           (set! (ref ret (+ 1 (* 4 iterations))) (real->encoded b))
           (set! (ref ret (+ 2 (* 4 iterations))) (real->encoded c))
           (set! (ref ret (+ 3 (* 4 iterations))) (char->integer #\=))))])

    ret))

;; string -> bv
(define-public (base64-string->bytevector string)
  (base64->bytevector
   (string->bytevector string (make-transcoder (latin-1-codec)))))

;; bv -> string
(define-public (bytevector->base64-string bv)
  (bytevector->string (bytevector->base64 bv)
                      (make-transcoder (latin-1-codec))))

;; string -> string
(define*-public (base64encode string optional: (transcoder (native-transcoder)))
  (bytevector->string
   (bytevector->base64 (string->bytevector string transcoder))
   (make-transcoder (latin-1-codec))))

;; string -> string
(define*-public (base64decode string optional: (transcoder (native-transcoder)))
  (bytevector->string
   (base64->bytevector
    (string->bytevector string (make-transcoder (latin-1-codec))))
   transcoder))
