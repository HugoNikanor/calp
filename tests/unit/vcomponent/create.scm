(define-module (test create)
  :use-module ((srfi srfi-1) :select (every))
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (-> sort*))
  :use-module ((hnh util table) :select (alist->table))
  :use-module ((vcomponent base) :select (vcomponent?))
  :use-module (vcomponent)
  :use-module ((vcomponent create)
               :select (create-vcomponent
                        with-parameters

                        as-list
                        vcalendar
                        vevent
                        vtimezone
                        standard
                        daylight))
  :use-module ((vcomponent)
               :select (children
                        properties
                        type
                        prop prop*
                        extract
                        param
                        vline?)))

;; vevent, vcalendar, vtimezone, standard, and daylight all trivial
;; and therefore not tested

(test-group "Empty component"
 (let ((ev (create-vcomponent 'TEST)))
   (test-equal 'TEST (type ev))
   (test-equal '() (children ev))
   (test-equal '() (properties ev))))

(test-group "Component with properties, but no children"
 (let ((ev (create-vcomponent 'TEST
                       prop: "value")))
   (test-equal '(PROP) (map car (properties ev)))
   (test-equal "value" (prop ev 'PROP))))

(test-group "Component with children, but no properties"
  (let* ((child (create-vcomponent 'CHILD))
         (ev (create-vcomponent 'TEST
                        (list child))))
    (test-equal '() (properties ev))
    (test-equal 1 (length (children ev)))
    ; (test-eq child (car (children ev)))
    ))

(test-group "Component with both children and properties"
  (let* ((child (create-vcomponent 'CHILD))
         (ev (create-vcomponent 'TEST
                         prop: "VALUE"
                         (list child))))
    (test-equal '(PROP) (map car (properties ev)))
    (test-equal "VALUE" (prop ev 'PROP))
    (test-equal 1 (length (children ev)))
    ; (test-eq child (car (children ev)))
    ))

(test-group "Component with multiple children"
  (let ((cal
         (vcalendar
          calscale: "GREGORIAN"
          (list
           (vevent summary: "Child 1")
           (vevent summary: "Child 2")))))
    (test-equal 2 (length (children cal)))
    (test-equal "GREGORIAN" (-> cal (prop 'CALSCALE)))
    (let ((ch (sort* (children cal)
                     string<? (extract 'SUMMARY))))
      (test-equal "Child 1" (-> ch (list-ref 0) (prop 'SUMMARY)))
      (test-equal "Child 2" (-> ch (list-ref 1) (prop 'SUMMARY))))))

(test-group "Component with no children, where last elements value is a list"
  (let ((ev (create-vcomponent 'TEST prop: (list 1 2 3))))
    (test-equal '() (children ev))
    (test-equal '(PROP) (map car (properties ev)))
    (test-equal '(1 2 3) (prop ev 'PROP))))

(test-group "With parameters"
  (let ((ev (create-vcomponent 'TEST
                        prop: (with-parameters param: 1 2))))
    (test-equal 2 (prop ev 'PROP))
    (test-equal '(1) (param (prop* ev 'PROP) 'PARAM))))

(test-group "As list"
  (let ((ev (create-vcomponent 'TEST
                        prop: (as-list (list 1 2 3)))))
    (test-equal '(1 2 3) (prop ev 'PROP))
    (test-equal 3 (length (prop* ev 'PROP)))
    (test-assert (every vline? (prop* ev 'PROP)))))

(test-group "List and parameters"
  (let ((ev
         (vevent
          prop: (as-list
                 (list
                  "One"
                  (with-parameters lang: "sv" "Två")
                  (with-parameters numeric: "3" "Three"))))))
    (test-equal 3 (length (prop* ev 'PROP)))
    (test-equal '("One" "Två" "Three") (prop ev 'PROP))
    (test-assert (every vline? (prop* ev 'PROP)))
    (test-equal (list (vline key: 'PROP
                             vline-value: "One")
                      (vline key: 'PROP
                             vline-value: "Två"
                             vline-parameters:
                             (alist->table '((LANG . "sv"))))
                      (vline key: 'PROP
                             vline-value: "Three"
                             vline-parameters:
                             (alist->table '((NUMERIC . "3")))))
      (prop* ev 'PROP))))


(test-error "Fail on nested with-parameters"
  'wrong-type-arg
  (vevent prop: (with-parameters a: "1"
                                 (with-parameters b: "2"
                                                  "3"))))

(test-group "An empty as-list is effectively the same as not having the property"
  (let ((ev (vevent prop: (as-list '()))))
    (test-equal '() (properties ev))))

(test-error "Fail on nested as-list"
  'wrong-type-arg
  (vevent prop: (as-list (list (as-list '())))))

(test-error "Fail on as-list inside with-parameters"
  'wrong-type-arg
  (vevent prop: (with-parameters a: "1"
                                 (as-list '()))))


(test-assert (vcomponent? (vcalendar)))
(test-eq 'VCALENDAR (type (vcalendar)))

(test-assert (vcomponent? (vevent)))
(test-eq 'VEVENT (type (vevent)))

(test-assert (vcomponent? (vtimezone)))
(test-eq 'VTIMEZONE (type (vtimezone)))

(test-assert (vcomponent? (standard)))
(test-eq 'STANDARD (type (standard)))

(test-assert (vcomponent? (daylight)))
(test-eq 'DAYLIGHT (type (daylight)))

'((vcomponent create))
