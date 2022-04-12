(define-module (test uuid)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (hnh util uuid))

(set! (@@ (hnh util uuid) %seed)
  (seed->random-state 0))

(test-equal "UUIDv4 fixed seed"
  "d19c9347-9a85-4432-a876-5fb9c0d24d2b"
  (uuid-v4))
