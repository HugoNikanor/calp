(define-module (test uuid)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (hnh util uuid))


(test-equal "UUIDv4 fixed seed"
  (let ((version (version)))
    (cond ((string=? version "2.2.7")
           "d19c9347-9a85-4432-a876-5fb9c0d24d2b")
          ((string=? version "3.0.9")
           "d19c9347-9a85-4432-a876-5fb9c0d24d2b")
          (else
           "Randomness isn't stable between guile versions")))
  (begin
    (parameterize ((seed (seed->random-state 0)))
     (uuid-v4))))
