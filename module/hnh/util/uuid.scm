(define-module (hnh util uuid)
  :use-module (ice-9 format)
  :export (seed uuid uuid-v4))

(define seed (make-parameter (random-state-from-platform)))

(define (uuid-v4)
  (define version 4)
  (define variant #b10)
  (format #f "~8'0x-~4'0x-~4'0x-~4'0x-~12'0x"
          (random (ash 1 (* 4 8)) (seed))
          (random (ash 1 (* 4 4)) (seed))
          (logior (ash version (* 4 3))
                  (random (1- (ash 1 (* 4 3))) (seed)))
          (logior (ash variant (+ 2 (* 4 3)))
                  (random (ash 1 (+ 2 (* 4 3))) (seed)))
          (random (ash 1 (* 4 12)) (seed))))

(define uuid uuid-v4)
