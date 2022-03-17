;;; Commentary:
;; Test that Base64 encoding and decoding works
;; Examples from RFC4648
;;; Code:

(define-module (test base64)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (base64))

;; Tests borrowed directly from RFC4648
(test-equal "" (base64encode ""))
(test-equal "Zg==" (base64encode "f"))
(test-equal "Zm8=" (base64encode "fo"))
(test-equal "Zm9v" (base64encode "foo"))
(test-equal "Zm9vYg==" (base64encode "foob"))
(test-equal "Zm9vYmE=" (base64encode "fooba"))
(test-equal "Zm9vYmFy" (base64encode "foobar"))
(test-equal "" (base64decode ""))
(test-equal "f" (base64decode "Zg=="))
(test-equal "fo" (base64decode "Zm8="))
(test-equal "foo" (base64decode "Zm9v"))
(test-equal "foob" (base64decode "Zm9vYg=="))
(test-equal "fooba" (base64decode "Zm9vYmE="))
(test-equal "foobar" (base64decode "Zm9vYmFy"))


;; Other tests

;; TODO normalize base64 errors

(test-error "Invalid base64"
  'decoding-error
  (base64decode "@@@@"))

(test-error "To short base64"
  'out-of-range
  (base64decode "="))

(test-equal "AAECAw==" (bytevector->base64-string #vu8(0 1 2 3)))

(test-equal #vu8(0 1 2 3) (base64-string->bytevector "AAECAw=="))
