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
  :use-module ((hnh util serialize) :select (serialize))
  :use-module ((vcomponent create) :select (vevent vcalendar with-parameters)))




(define ev
  (-> (vcomponent type: 'DUMMY)
      (set (prop* 'X-KEY) (just (list (vline value: "dummy value"))))))

(test-eqv "Non-existant properties return #f"
  #f (prop1 ev 'MISSING))

(test-assert "Existing property is non-false"
  (prop1 ev 'X-KEY))

(test-equal "Getting value of existing property"
  "dummy value" (prop1 ev 'X-KEY))

(define calendar (add-child (vcomponent type: 'VCALENDAR)
                            ev))

(test-equal 1 (length (vcomponent-children calendar)))

;;; TODO remove child
;; (abandon! calendar ev)
;; (test-equal 0 (length (children calendar)))



(define vline*
  (vline
   value: (date year: 2020 month: 01 day: 02)
   params: (alist->table '((VALUE . "DATE")))))

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
  `((KEY . ,(list (vline value: "Value"))))
  (table->list
   (vcomponent-properties
    (set vcomponent* (prop* 'KEY) (just (list (vline value: "Value")))))))

(test-equal "VLine serialization"
  '(vline value: "Value")
  (serialize (vline value: "Value") ))

(test-equal "VLine with parameters serialization"
  '(vline value: "Value"
          params: (-> (table) (table-put 'a "1")))
  (serialize (vline value: "Value"
                    params: (alist->table '((a . "1"))))))

;;; NOTE serialization test for vcomponent is omitted.
;;; This since the order of properties isn't stable
;;; (or at least won't be until `table` is rewritten).
;;; Also, the serialization format is used for the
;;; application/vnd.guile-read media type, meaning that it's
;;; effectively tested either way.


'((vcomponent))
