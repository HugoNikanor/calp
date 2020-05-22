(define-module (global))

;; TODO encapsulating these in
;; atomic boxes might be a good
;; idea.

(define-once basedir #f)
(export basedir)

(define-once *prodid* "-//hugo//Calparse 0.9//EN")
(export *prodid*)

