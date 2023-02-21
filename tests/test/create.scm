(define-module (test create)
  :use-module ((srfi srfi-1) :select (every))
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (vcomponent create)
  :use-module (vcomponent))

;; vevent, vcalendar, vtimezone, standard, and daylight all trivial
;; and therefore not tested

(test-group "Empty component"
 (let ((ev (vcomponent 'TEST)))
   (test-equal 'TEST (type ev))
   (test-equal '() (children ev))
   (test-equal '() (properties ev))))

(test-group "Component with properties, but no children"
 (let ((ev (vcomponent 'TEST
                       prop: "value")))
   (test-equal '(PROP) (map car (properties ev)))
   (test-equal "value" (prop ev 'PROP))))

(test-group "Component with children, but no properties"
  (let* ((child (vcomponent 'CHILD))
         (ev (vcomponent 'TEST
                        (list child))))
    (test-equal '() (properties ev))
    (test-equal 1 (length (children ev)))
    (test-eq child (car (children ev)))))

(test-group "Component with both children and properties"
  (let* ((child (vcomponent 'CHILD))
         (ev (vcomponent 'TEST
                         prop: "VALUE"
                         (list child))))
    (test-equal '(PROP) (map car (properties ev)))
    (test-equal "VALUE" (prop ev 'PROP))
    (test-equal 1 (length (children ev)))
    (test-eq child (car (children ev)))))

(test-group "Component with no children, where last elements value is a list"
  (let ((ev (vcomponent 'TEST prop: (list 1 2 3))))
    (test-equal '() (children ev))
    (test-equal '(PROP) (map car (properties ev)))
    (test-equal '(1 2 3) (prop ev 'PROP))))

(test-group "With parameters"
  (let ((ev (vcomponent 'TEST
                        prop: (with-parameters param: 1 2))))
    (test-equal 2 (prop ev 'PROP))
    (test-equal '(1) (param (prop* ev 'PROP) 'PARAM))))

(test-group "As list"
  (let ((ev (vcomponent 'TEST
                        prop: (as-list (list 1 2 3)))))
    (test-equal '(1 2 3) (prop ev 'PROP))
    (test-equal 3 (length (prop* ev 'PROP)))
    (test-assert (every vline? (prop* ev 'PROP)))))

;; (test-group "Parameters and lists" )
