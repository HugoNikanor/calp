;;; Commentary:
;; Test base functionallity of vcomponent structures.
;;; Code:

(define-module (test vcomponent)
  :use-module (srfi srfi-17)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util table)
  :use-module (datetime)
  :use-module (vcomponent base))




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
   vline-value: #2020-01-02
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

;; remove-property

;; extract extract*


;; remove-parameter
;; value
;; param

;; parameters
;; properties

;; x-property?
;; internal-field?
