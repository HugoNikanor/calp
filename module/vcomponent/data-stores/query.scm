;;; Commentary:
;;; This are common helper methods for querying calendar stores.
;;; It should probably be merged into (vcomponent data-stores common)
;;; Code:
(define-module (vcomponent data-stores query)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-88)
  :use-module (vcomponent)
  :use-module (vcomponent datetime)
  :use-module (vcomponent data-stores common)
  :use-module (datetime)
  :use-module (ice-9 match)
  :use-module (ice-9 format)
  :use-module (hnh util)
  :use-module (hnh util type)
  :export (entries-between))

(define (make-counter)
  (let ((c 0))
    (lambda ()
      (set! c (1+ c))
      c)))

(define counter (make-counter))

(define query-cache (make-hash-table))

;;; This is just a helper around the `entries-in-interval` procedure on stores.
;;; This procedure should eventually be removed in favour of doing everything in stores.
;;; Possibly to be replaced with a multi-dispatch search operation for multiple stores.
;;; 
;;; TODO rework this to a general operation for querying multiple stores at once
;;; Has same return type as `entries-in-interval` (store-name href vcalendar?)
;;; 
;;; NOTE: this is a rather slow procedure despite returning streams.
;;; This is since most stores pre-fetch everything, and simply wrap it
;; into a stream
(define (entries-between reference-zone start end . stores)
  (typecheck reference-zone string?)
  (typecheck start zoned-datetime?)
  (typecheck end   zoned-datetime?)
  (typecheck stores (list-of (pair-of string? calendar-data-store?)))

  ;; TODO this wants entries in the interval [start, end), where start to end is in a specific timezone. Transpose them to UTC, and get all events in THAT interval.


  ;; TODO query caching MUST be done on store level, since only they know how to invalidate caches.
  ;; It's only done here due to temporary lazyness
  ;; TODO tz
  (cond ((hash-ref query-cache (cons start end)) => identity)
        (else
         ;; TODO log level info or debug
         (define id (counter))
         (format (current-error-port)
                 "<DEBUG> [~a] Querying ~s - ~s from ~{~s~^, ~}~%"
                 id start end (map car stores))

         (define result
           (stream-map
            (lambda (pair)
              (let ((record (cdr pair)))
                (list (car pair) (vector-ref record 2) (vector-ref record 3))))
            (interleave-streams
             (lambda (a b)
               ;; (format (current-error-port) "a: ~s, b: ~s~%" a b)
               (datetime<
                (vector-ref (cdr a) 0)
                (vector-ref (cdr b) 0)))
             ;; stream of (tuple-of store-id href vcalendar)
             (map (lambda (store-pair)
                    (stream-map
                     (lambda (record) (cons (car store-pair) record))
                     (entries-in-interval (cdr store-pair)
                                          reference-zone start end)))
                  stores))))

         ;; TODO log level debug
         (format (current-error-port) "<DEBUG> [~a] finished: ~s~%"
                 id result)

         (hash-set! query-cache (cons start end) result)

         result)))
