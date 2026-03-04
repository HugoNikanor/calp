;;; Commentary:
;;; This file does NOT declare a media type, but instead contains
;;; common operation for calendar types.
;;; Code:

(define-module (vcomponent media-type types)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
  :use-module (hnh util table)
  :use-module (web uri)
  :use-module (base64)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :use-module (vcomponent type period)
  :use-module ((datetime) :select (date? time? datetime?))
  :use-module ((vcomponent type duration)   :select (duration?))
  :use-module ((vcomponent type period)     :select (period?))
  :use-module ((vcomponent type recurrence) :select (recur-rule?))
  :use-module ((vcomponent type utc-offset) :select (utc-offset?))
  :export (
           default-types default-type
           apparent-types apparent-type
           ))


;;; Table mapping field names to their default types.
;;; Field names are given in as symbols all uppercase
;;; Types are given as symbols in all uppercase
(define-once default-types
  (make-parameter
   (fold (lambda (entry default-types)
           (let ((type fields (car+cdr entry)))
             (fold (lambda (field default-types)
                     (table-put default-types field type))
                   default-types
                   fields)))
         (table)
         `((DATE-TIME COMPLETED DTEND DUE DTSTART RECURRENCE-ID CREATED DTSTAMP
                      LAST-MODIFIED ACKNOWLEDGED EXDATE)
           (DURATION TRIGGER DURATION)
           (PERIOD FREEBUSY)

           ;; General text types. Many of them have further confines on what
           ;; is "valid" values, but no special types are required.
           ;; Many of these are "extensible enums", which means that they
           ;; have a number of pre-defined values, but allow extensions
           ;; through future standards or user extensions.
           (TEXT METHOD PRODID COMMENT DESCRIPTION LOCATION SUMMARY
                 TZNAME CONTACT RELATED_TO UID CATEGORIES RESOURCES
                 CLASS ACTION)

           ;; special handling, but not in way which matters
           (TEXT TZID)

           ;; Special handling
           (TEXT REQUEST-STATUS VERSION)

           ;; Strict enum types, could actually be validated here
           (TEXT TRANSP PARTSTAT CALSCALE STATUS)

           (UTC-OFFSET TZOFFSETFROM TZOFFSETTO)

           (URI ATTACH TZURL URL)

           (INTEGER PERCENT-COMPLETE PRIORITY REPEAT SEQUENCE)

           ;; Special handling
           (FLOAT GEO)

           (RECUR RRULE)

           (CAL-ADDRESS ORGANIZER ATTENDEE)

           ;; RFC 7986 (New Properties for iCalendar)
           ;; The RFC also registers a couple more fields, which
           ;; explicitly lacks default type. DO NOT add them to this table.
           (TEXT NAME COLOR)

           ;; Common extensions:
           ;; [MS_OXICIAL]: 2.1.3 Processing rules (https://learn.microsoft.com/en-us/openspecs/exchange_server_protocols/ms-oxcical/74d3bf60-f30d-4fca-84d3-cfd04da8e627), read 2025-12-02
           ;; NOTE that the "standard" has more registered properties
           ;; than these. This is just the most commonly used subset
           (DATE-TIME X-CALEND X-CALSTART
                      X-CLIPEND X-CLIPSTART)
           (CAL-ADDRESS X-OWNER)
           (DURATION X-PUBLISHED-TTL)
           (TEXT X-WR-CALDESC X-WR-CALNAME
                 X-ALT-DESC)
           ))))

;;; Get default type for the given field name
(define (default-type key)
  (table-get (default-types) key))

;;; Assoc list from Scheme type predicates, to ical type names.
(define-once apparent-types
  (make-parameter
   (list
    (cons (@ (scheme base) bytevector?)     'BINARY)
    (cons boolean?        'BOOLEAN)
    (cons (lambda (v) (and (uri? v) (eq? 'mailto (uri-scheme v))))
                          'CAL-ADDRESS)
    (cons date?           'DATE)
    (cons datetime?       'DATE-TIME)
    (cons duration?       'DURATION)
    (cons (lambda (v) (and (rational? v) (inexact? v)))
                          'FLOAT)
    (cons exact-integer?  'INTEGER)
    (cons period?         'PERIOD)
    (cons recur-rule?     'RECUR)
    (cons string?         'TEXT)
    (cons time?           'TIME)        ; TODO utc
    (cons (lambda (v) (and (uri? v) (not (eq? 'mailto (uri-scheme v)))))
                          'URI)
    (cons utc-offset?     'UTC-OFFSET)

    ;; unknown? MUST NOT be added here.
    ;; If it where added here, it would be treated as an actual type,
    ;; and the mechanism to ensure that the VALUE parameter is
    ;; preserved would break.
    )))

;;; Get the apparent iCalendar type of the given Scheme value.
(define (apparent-type value)
  (predicate-list-get (apparent-types) value))
