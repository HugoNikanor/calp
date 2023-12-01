;;; Commentary:
;; Test base functionallity of vcomponent structures.
;;; Code:

(define-module (test vcomponent)
  :use-module (srfi srfi-17)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util table)
  :use-module (datetime)
  :use-module (vcomponent base)
  :use-module ((vcomponent create) :select (vevent vcalendar with-parameters)))




(define ev
  (prop (vcomponent type: 'DUMMY)
        'X-KEY "value"))

(test-eqv "Non-existant properties return #f"
  #f (prop ev 'MISSING))

(test-assert "Existing property is non-false"
  (prop ev 'X-KEY))

(test-equal "Getting value of existing property"
  "value" (prop ev 'X-KEY))

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
  1 (length (children (add-child vcomponent* child))))

(test-eqv
    "But the source isn't modified"
  0 (length (children vcomponent*)))

(test-equal "Setting property"
  (list (list 'KEY (vline key: 'KEY vline-value: "Value")))
  (properties
   (prop vcomponent* 'KEY "Value")))

(let ((vl (vline key: 'KEY vline-value: "Value")))
  (test-equal "Setting property vline"
    (list (list 'KEY vl))
    (properties
     (prop* vcomponent* 'KEY vl))))

(test-equal "Set properties test"
  '(K1 K2)
  (map car
   (properties
    (apply set-properties
           vcomponent*
           `((K1 . "V1")
             (K2 . "V2"))))))

(test-equal "VLine string representation"
  "(vline #:key KEY #:vline-value \"Value\")
"
  (with-output-to-string
    (lambda ()
      (write (vline key: 'KEY vline-value: "Value") ))))

(test-equal "VLine with parameters representation"
  "(vline #:key
       KEY
       #:vline-value
       \"Value\"
       #:vline-parameters
       (#:a \"1\"))
"
 (with-output-to-string
   (lambda ()
     (write (vline key: 'KEY
                   vline-value: "Value"
                   vline-parameters:
                   (alist->table '((a . "1"))))))))

(test-equal "VComponent string representation"
  "(vcomponent
  'VCALENDAR
  (list (vcomponent
          'VEVENT
          #:dtstart
          (with-parameters
            #:TZID
            \"Europe/Stockholm\"
            #2023-03-01T10:00:00)
          #:uid
          \"049d9004-cb1e-4c8d-bb54-042689d9808b\")))
"

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


;; remove-parameter
;; value
;; param

;; parameters
;; properties

(test-group "x-property?"
  (test-assert (x-property? 'X-Extension))
  (test-assert (not (x-property? 'Regular)))
  (test-assert (not (x-property? '-internal))))

(test-group "internal-field?"
  (test-assert (not (internal-field? 'X-Extension)))
  (test-assert (not (internal-field? 'Regular)))
  (test-assert (internal-field? '-internal)))

'((vcomponent base))
