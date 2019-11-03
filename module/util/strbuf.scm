;;; Description:
;; Alternative class to regular string, optimized for really fast appending,
;; Works on a byte level, and isn't really good for anything else.
;;; Code:

(define-module (util strbuf)
  :use-module (srfi srfi-9)
  :use-module (rnrs bytevectors)
  :use-module ((rnrs io ports)
               :select (bytevector->string native-transcoder))
  :use-module ((ice-9 optargs) :select (define*-public))
  )

(define-record-type <strbuf>
  (make-strbuf% len bytes)
  strbuf?
  (len get-length set-length!)
  (bytes get-bytes set-bytes!))

(define-public (make-strbuf)
  (make-strbuf% 0 (make-u8vector #x1000)))

(define (strbuf-realloc! strbuf)
  (let* ((len (u8vector-length (get-bytes strbuf)))
         (nv (make-u8vector (ash len 1))))
    (bytevector-copy! (get-bytes strbuf) 0
                      nv 0 len)
    (set-bytes! strbuf nv)))

;; TODO charset
(define*-public (strbuf->string strbuf #:optional
                                (transcoder (native-transcoder)))
  (let ((bv (make-u8vector (get-length strbuf))))
    (bytevector-copy! (get-bytes strbuf) 0
                      bv 0
                      (get-length strbuf))
    (bytevector->string bv transcoder)))

(define-public (strbuf-reset! strbuf)
  (set-length! strbuf 0))

(define-public (strbuf-append! strbuf u8)
  (catch 'out-of-range
    (lambda ()
     (u8vector-set! (get-bytes strbuf)
                    (get-length strbuf)
                    u8))
    (lambda (err . args)
      (strbuf-realloc! strbuf)
      (strbuf-append! strbuf u8)))
  (set-length! strbuf (1+ (get-length strbuf))))

