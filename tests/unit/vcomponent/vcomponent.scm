;;; Commentary:
;; Test base functionallity of vcomponent structures.
;;; Code:

(define-module (test vcomponent)
  :use-module (srfi srfi-17)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util table)
  :use-module (datetime)
  :use-module (vcomponent)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module ((vcomponent create) :select (vevent vcalendar with-parameters)))




(define ev
  (-> (vcomponent type: 'DUMMY)
      (set (prop* 'X-KEY) (just "value"))))

(test-eqv "Non-existant properties return #f"
  #f (prop1 ev 'MISSING))

(test-assert "Existing property is non-false"
  (prop1 ev 'X-KEY))

(test-equal "Getting value of existing property"
  "value" (prop1 ev 'X-KEY))

(define calendar (add-child (vcomponent type: 'VCALENDAR)
                            ev))

(test-equal 1 (length (children calendar)))

;;; TODO remove child
;; (abandon! calendar ev)
;; (test-equal 0 (length (children calendar)))



(define vline*
  (vline
   key: 'DTSTART
   vline-value: (date year: 2020 month: 01 day: 02)
   vline-parameters: (alist->table
                      '((VALUE . "DATE")))
   vline-source: "DTSTART;VALUE=DATE:2020-01-02"))

(test-group "vline"
 (test-assert "Type check works as expected"
   (vline? vline*)))

(define vcomponent*
  (vcomponent type: 'VEVENT))

(test-assert "Type check works as expected"
  (vcomponent? vcomponent*))

(define child
  (vcomponent type: 'CHILD))


(test-eqv
    "An added component extends length"
  1 (length (vcomponent-children (add-child vcomponent* child))))

(test-eqv
    "But the source isn't modified"
  0 (length (vcomponent-children vcomponent*)))

(test-equal "Setting property"
  `((KEY . ,(list (vline key: 'KEY vline-value: "Value"))))
  (vcomponent-properties
   (set vcomponent* (prop* 'KEY) (just "Value"))))

(let ((vl (vline key: 'KEY vline-value: "Value")))
  (test-equal "Setting property vline"
    `((KEY ,vl))
    (vcomponent-properties
     (set vcomponent* (prop* 'KEY) (just vl)))))

(test-equal "Set properties test"
  '(K1 K2)
  (map car
   (vcomponent-properties
    (-> vcomponent*
        (set (prop* 'K1) (just "V1"))
        (set (prop* 'K2) (just "V2"))))))

(test-equal "VLine string representation"
  "#.(vline #:key 'KEY #:vline-value \"Value\")"
  (with-output-to-string
    (lambda ()
      (write (vline key: 'KEY vline-value: "Value") ))))

(test-equal "VLine with parameters representation"
  "#.(vline #:key
         'KEY
         #:vline-value
         \"Value\"
         #:vline-parameters
         (-> (table) (table-put 'a \"1\")))"
 (with-output-to-string
   (lambda ()
     (write (vline key: 'KEY
                   vline-value: "Value"
                   vline-parameters:
                   (alist->table '((a . "1"))))))))

(test-equal "VComponent string representation"
  "#.(create-vcomponent
   'VCALENDAR
   (list (create-vcomponent
          'VEVENT
          #:dtstart
          (with-parameters
           #:TZID
           \"Europe/Stockholm\"
           (datetime #:date #2023-03-01 #:time #10:00:00 #:tz #f))
          #:uid
          \"049d9004-cb1e-4c8d-bb54-042689d9808b\")))"

  (with-output-to-string
    (lambda ()
      (write (vcalendar
              ;; name: "Hello"
              (list (vevent
                     uid: "049d9004-cb1e-4c8d-bb54-042689d9808b"
                     dtstart:
                     (with-parameters
                      tzid: "Europe/Stockholm"
                      (datetime year: 2023 month: mars day: 1
                                hour: 10)))))))))

;; remove-property

;; extract extract*


;; value
;; param

;; parameters
;; properties

'((vcomponent))
