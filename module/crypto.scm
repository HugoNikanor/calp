(define-module (crypto)
  :use-module (rnrs bytevectors)
  :use-module (system foreign)
  :use-module (ice-9 format)
  :export (sha256 checksum->string))

(define-once libcrypto (dynamic-link "libcrypto"))

(define SHA_DIGEST_LENGTH       20)
(define SHA224_DIGEST_LENGTH    28)
(define SHA256_DIGEST_LENGTH    32)
(define SHA384_DIGEST_LENGTH    48)
(define SHA512_DIGEST_LENGTH    64)

(define SHA256
 ((@ (system foreign) pointer->procedure)
  '* (dynamic-func "SHA256" libcrypto)
  `(* ,(@ (system foreign) size_t) *)))

(define (sha256 msg)
  (define md (make-bytevector SHA256_DIGEST_LENGTH))
  (define bv
    (cond ((bytevector? msg) msg)
          ((string? msg) (string->utf8 msg))
          (else (throw 'value-error "Invalid type"))))
  (SHA256 ((@ (system foreign) bytevector->pointer) bv)
          (bytevector-length bv)
          ((@ (system foreign) bytevector->pointer) md))
  md)

(define (checksum->string md)
  (string-concatenate
   (map (lambda (byte)
          (format #f "~x~x"
                  (logand #xF (ash byte -4))
                  (logand #xF byte)))
        (bytevector->u8-list md))))

