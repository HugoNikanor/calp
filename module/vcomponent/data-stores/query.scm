;;; Commentary:
;;; This are common helper methods for querying calendar stores.
;;; It should probably be merged into (vcomponent data-stores common)
;;; Code:
(define-module (vcomponent data-stores query)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-88)
  :use-module (vcomponent)
  :use-module (vcomponent data-stores common)
  :use-module (datetime)
  :use-module (ice-9 match)
  :use-module (ice-9 format)
  :use-module (hnh util)
  :use-module (hnh util type)
  :export (entries-between))

(define counter
  (let ((c 0))
    (lambda ()
      (set! c (1+ c))
      c)))

(define query-cache (make-hash-table))

;;; TODO rework this to a general operation for querying multiple stores at once
;;; Has same return type as `entries-in-interval` (store-name href vcalendar?)

;;; NOTE: this is a rather slow procedure despite returning streams.
;;; This is since most stores pre-fetch everything, and simply wrap it
;; into a stream
(define (entries-between start end . stores)
  (typecheck start (or date? datetime?))
  (typecheck end   (or date? datetime?))
  (typecheck stores (list-of (pair-of string? calendar-data-store?)))


  ;; TODO query caching MUST be done on store level, since only they know how to invalidate caches.
  ;; It's only done here due to temporary lazyness
  (cond ((hash-ref query-cache (cons start end)) => identity)
        (else
         ;; TODO log level info or debug
         (define id (counter))
         (format (current-error-port)
                 "<DEBUG> [~a] Querying ~s - ~s from ~{~s~^, ~}~%"
                 id start end (map car stores))

         (define result
           (interleave-streams
            (match-lambda* (((_ _ a) (_ _ b))
                            (datetime<?
                             (as-datetime (prop1 (car (vcomponent-children a)) 'DTSTART))
                             (as-datetime (prop1 (car (vcomponent-children b)) 'DTSTART)))))
            ;; stream of (tuple-of store-id href vcalendar)
            (map (lambda (store-pair)
                   (stream-map
                    (lambda (p) (list (car store-pair) (car p) (cdr p)))
                    (entries-in-interval (cdr store-pair)
                                         (as-datetime start)
                                         ;; TODO +1 day?
                                         (as-datetime end))))
                 stores)))

         ;; TODO log level debug
         (format (current-error-port) "<DEBUG> [~a] finished: ~s~%"
                 id result)

         (hash-set! query-cache (cons start end) result)

         result)))
