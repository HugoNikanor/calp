(define-module (test create)
  :use-module ((srfi srfi-1) :select (every))
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (-> sort*))
  :use-module ((hnh util table) :select (alist->table table->list))
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module ((vcomponent) :select (vcomponent?))
  :use-module (vcomponent)
  :use-module ((vcomponent create)
               :select (create-vcomponent
                        with-parameters

                        vcalendar
                        vevent
                        vtimezone
                        standard
                        daylight))
  :use-module ((vcomponent)
               :select (vcomponent-children
                        vcomponent-properties
                        type

                        prop*
                        extract1

                        vline

                        vline?
                        )))

;; vevent, vcalendar, vtimezone, standard, and daylight all trivial
;; and therefore not tested

(test-group "Empty component"
 (let ((ev (create-vcomponent 'TEST)))
   (test-equal 'TEST (type ev))
   (test-equal '() (vcomponent-children ev))
   (test-equal '() (table->list (vcomponent-properties ev)))))

(test-group "Component with properties, but no children"
 (let ((ev (create-vcomponent 'TEST
                       prop: "value")))
   (test-equal '(PROP) (table->list (vcomponent-properties ev) (lambda (a _) a)))
   (test-equal "value" (get ev (prop* 'PROP) just* car* vline-value*))))

(test-group "Component with children, but no properties"
  (let* ((child (create-vcomponent 'CHILD))
         (ev (create-vcomponent 'TEST
                        (list child))))
    (test-equal '() (table->list (vcomponent-properties ev)))
    (test-equal 1 (length (vcomponent-children ev)))
    ; (test-eq child (car (vcomponent-children ev)))
    ))

(test-group "Component with both children and properties"
  (let* ((child (create-vcomponent 'CHILD))
         (ev (create-vcomponent 'TEST
                         prop: "VALUE"
                         (list child))))
    (test-equal '(PROP) (table->list (vcomponent-properties ev) (lambda (a _) a)))
    (test-equal "VALUE" (get ev (prop* 'PROP) just* car* vline-value*))
    (test-equal 1 (length (vcomponent-children ev)))
    ; (test-eq child (car (vcomponent-children ev)))
    ))

(test-group "Component with multiple children"
  (let ((cal
         (vcalendar
          calscale: "GREGORIAN"
          (list
           (vevent summary: "Child 1")
           (vevent summary: "Child 2")))))
    (test-equal 2 (length (vcomponent-children cal)))
    (test-equal "GREGORIAN" (prop1 cal 'CALSCALE))
    (let ((ch (sort* (vcomponent-children cal)
                     string<? (extract1 'SUMMARY))))
      (test-equal "Child 1" (-> ch (list-ref 0) (prop1 'SUMMARY)))
      (test-equal "Child 2" (-> ch (list-ref 1) (prop1 'SUMMARY))))))

(test-group "Component with no children, where last elements value is a list"
  (let ((ev (create-vcomponent 'TEST prop: (list 1 2 3))))
    (test-equal '() (vcomponent-children ev))
    (test-equal '(PROP) (table->list (vcomponent-properties ev) (lambda (a _) a)))
    (test-equal (just (list (vline value: 1)
                            (vline value: 2)
                            (vline value: 3)))
      (get ev (prop* 'PROP)))))


(test-group "With parameters"
  (let ((ev (create-vcomponent 'TEST
                        prop: (with-parameters param: 1 2))))
    (test-equal 2 (prop1 ev 'PROP))
    (test-equal (just 1) (get ev (prop* 'PROP) just* car* (param* 'PARAM)))))


(test-group "As list"
  (let ((ev (create-vcomponent 'TEST
                               prop: (list 1 2 3))))
    (test-equal 1 (prop1 ev 'PROP))
    (test-equal 3 (length (get ev (prop* 'PROP) just*)))
    (test-assert (every vline? (get ev (prop* 'PROP) just*)))))


(test-group "List and parameters"
  (let ((ev
         (vevent
          prop: (list
                 "One"
                 (with-parameters lang: "sv" "Två")
                 (with-parameters numeric: "3" "Three")))))
    (test-equal 3 (length (get ev (prop* 'PROP) just*)))
    (test-equal '("One" "Två" "Three") (map vline-value (get ev (prop* 'PROP) just*)))
    (test-assert (every vline? (get ev (prop* 'PROP) just*)))
    (test-equal (list (vline value: "One")
                      (vline value: "Två"
                             params: (alist->table '((LANG . "sv"))))
                      (vline value: "Three"
                             params: (alist->table '((NUMERIC . "3")))))
      (get ev (prop* 'PROP) just*))))


(test-error "Fail on nested with-parameters"
  'wrong-type-arg
  (vevent prop: (with-parameters a: "1"
                                 (with-parameters b: "2"
                                                  "3"))))

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
