;;; Commentary:
;;; A virtual data store which uses other data stores for its storage.
;;; Used to merge stores into larger stores
;;; Code:

(define-module (vcomponent data-stores meta)
  :use-module (oop goops)
  :use-module (vcomponent data-stores common)
  :use-module ((srfi srfi-88) :select ())
  :export ()
  )

(define-class <meta-data-store> (<calendar-data-store>)
  (stores accessor: stores
          init-value: '()))

(define-method (get-calendar ))
