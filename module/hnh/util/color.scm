;;; Commentary:
;;; A basic common color interface.
;;; Currently, only hsl(a), rgb(a), (and css color names) are
;;; provided, with limited support for converting between different forms.
;;; TODO select the most generic colorspace possible (possibly XYZ),
;;; and has that as a common "ancestor" to all color objects. Allow
;;; new color representations to be added by them adding two procedures:
;;; one for converting to that space from the common, and one from the
;;; common to that specific space.
;;; Code:
(define-module (hnh util color)
  :use-module (srfi srfi-88)
  :use-module (hnh util object)
  :use-module (hnh util type)
  :use-module (ice-9 format)
  :export (color-rgba
           color-rgba?
           rgba-r rgba-r*
           rgba-g rgba-g*
           rgba-b rgba-b*
           rgba-a rgba-a*

           color-hsla
           color-hsla?
           hsla-h hsla-h
           hsla-s hsla-s
           hsla-l hsla-l
           hsla-a hsla-a

           color?

           rgb->hsl
           hsl->rgb
           ->rgb
           ->rgb/values

           rgb->hex
           ))

(define (in-interval x min max)
  (<= min x max))

(define-type (color-rgba)
  (rgba-r keyword: r type: (and exact-integer? (in-interval 0 255)))
  (rgba-g keyword: g type: (and exact-integer? (in-interval 0 255)))
  (rgba-b keyword: b type: (and exact-integer? (in-interval 0 255)))
  (rgba-a keyword: a type: (and real? (in-interval 0 1))
          default: 1))

(define-type (color-hsla)
  (hsla-h keyword: h type: (and exact-integer? (in-interval 0 360)))
  (hsla-s keyword: s type: (and real? (in-interval 0 1)))
  (hsla-l keyword: l type: (and real? (in-interval 0 1)))
  (hsla-a keyword: a type: (and real? (in-interval 0 1))
          default: 1))

(define (color? x)
  (or (color-rgba? x)
      (color-hsla? x)))

(define (rgb->hsl c)
  (throw 'not-implemented "rgb->hsl"))

(define (hsl->rgb c)
  (define (f n)
    (let* ((k (modulo (+ n (/ (hsla-h c) 30)) 12))
           (a (* (hsla-s c) (min (hsla-l c) (- 1 (hsla-l c))))))
      (inexact->exact
       (round
        (* 255
           (- (hsla-l c) (* a (max -1 (min 1 (- k 3) (- 9 k))))))))))
  (color-rgba
   r: (f 0)
   g: (f 8)
   b: (f 4)
   a: (hsla-a c)))

(define (rgb->hex c)
  (typecheck c color-rgba?)
  (with-output-to-string
    (lambda ()
      (display #\#)
      (format #t "~2'0x~2'0x~2'0x"
              (rgba-r c)
              (rgba-g c)
              (rgba-b c))
      (unless (= 1 (rgba-a c))
        (format #t "~2'0x"
                (inexact->exact
                 (round (* 255 (rgba-a c)))))))))

(define (->rgb c)
  (cond ((color-rgba? c) c)
        ((color-hsla? c) (hsl->rgb c))
        (else (scm-error 'misc-error "->rgb"
                         "Can't convert to rgb value: ~s"
                         (list c) #f))))

(define (->rgb/values c)
  (let ((rgb (->rgb c)))
    (values (rgba-r rgb)
            (rgba-g rgb)
            (rgba-b rgb)
            (rgba-a rgb))))
