;;; Commentary:
;;; A virtual data store which uses other data stores for its storage.
;;; Used to merge stores into larger stores
;;; Code:

(define-module (vcomponent data-stores meta)
  :use-module (oop goops)
  :use-module (vcomponent data-stores common)
  :use-module (srfi srfi-41)
  :use-module ((srfi srfi-88) :select ())
  :export ()
  )

(define-class <meta-data-store> (<calendar-data-store>)
  (stores accessor: stores
          init-value: '()
          init-keyword: stores:))



(define-method (get-all (this <meta-data-store>))
  (map get-all (stores this)))

(define-method (get-by-uid (this <meta-data-store>) (uid <string>))
  (stream-car
   (stream-append
    (steam-map (lambda (store) (get-by-uid store uid))
               (list->stream (stores this)))
    (stream #f))))
