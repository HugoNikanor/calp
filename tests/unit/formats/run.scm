(define-module (test formats run)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (->))
  :use-module (hnh util path)
  :use-module ((ice-9 rdelim) :select (read-string))
  :use-module (ice-9 pretty-print)
  :use-module (rnrs io ports)
  :use-module (datetime)
  :use-module (vcomponent create)
  :use-module ((vcomponent formats ical) :prefix #{ics:}#)
  :use-module ((vcomponent formats xcal) :prefix #{xcs:}#)
  :use-module ((vcomponent formats sxcal) :prefix #{sxcs:}#)
  :use-module (sxml namespaced)
  :use-module ((calp namespaces) :select (xcal))
  :use-module (hnh test xmllint)

  ;; Requirements for the reference component
  :use-module ((hnh util) :select (->))
  :use-module (datetime)
  :use-module (vcomponent create)
  :use-module (rnrs io ports)
  )

;;; Reference component. This component should be built to
;;; contain all weird cases which may be encountered.
(define ev
 (vcalendar
  calscale: "GREGORIAN"
  ;; method: ""
  prodid: "-//CALP-TEST//x.y"
  version: "2.0"
  (list
   (vevent
    attach: (with-parameters fmttype: "text/plain"
                             encoding: "BASE64"
                             value: "BINARY"
                             (-> "\n"
                                 (string->bytevector
                                  (make-transcoder (utf-8-codec)))))
    ;; categories: '("a" "b")
    class: 'PUBLIC
    comment: "A comment"
    description: "Descrition of the event"
    description: (with-parameters language: "sv" "Beskrivning av händelsen")
    ;; geo: (geo y: 10 x: 20)
    location: "Room 5"
    priority: 5
    ;; resources:
    status: 'CANCELLED
    summary: "Event summary"
    completed: (datetime year: 2023 month: may day: 10 hour: 10 minute: 20)
    dtstart: (datetime year: 2023 month: may day: 1)
    uid: "e4e812b8-dbb9-438d-ba56-ab58321fe4e1"
    ;; dtend: (date year: 2023 month: may day: 5)
    ;; TODO duration (on another component)
    ;; freebusy:
    ;; trasp: 'TRANSPARENT
    ))))



(define* (run-test name reference
                   key:
                   serialize
                   parse)

  ;; Assert serialize is set

  (define target
    (call-with-input-file (path-append (dirname (current-filename))
                                       reference)
      read-string))

  (define serialized-component
    (call-with-output-string
      (lambda (port) (serialize ev port))))

  (test-equal (string-append "serialise " name)
    target serialized-component)

  (when parse
   (test-equal (string-append "parse " name)
     (list ev)
     (call-with-input-string serialized-component parse))))



;;; Currently many of these have some extra baggage in their
;;; serialise or parse forms. This should be kept to a minimum,
;;; to ensure that all implementations are compatible.
;;; However, reflowing data for better diffs is acceptable.

;;; TODO remove this.
;;; iCalendar disabled since the internal vcomponent format isn't yet
;;; fully defined.
(test-skip "iCalendar")

(test-group "iCalendar"
  (run-test
   "iCalendar" "target.ics"
   serialize: ics:serialize
   parse: ics:deserialize))

(test-group "sxCalendar"
  (run-test
   "sxCalendar" "target.sxml"
   serialize:
   (lambda (ev p)
     (pretty-print
      (namespaced-sxml->sxml
       ((@@ (vcomponent formats sxcal) serialize/object) ev)
       `((,xcal . xcal)))
      p))))

(test-group "xCalendar"
  (run-test
   "xCalendar" "target.xml"
   serialize: (lambda (ev p)
                (-> (call-with-output-string
                      (lambda (port) (xcs:serialize ev port)))
                    xmllint
                    (display p)))))



'((vcomponent formats xcal)
  (vcomponent formats xcal output)
  (vcomponent formats xcal parse)
  (vcomponent formats xcal types)

  (vcomponent formats ical)
  (vcomponent formats ical output)
  (vcomponent formats ical parse)
  (vcomponent formats ical types))
