(define-module (test object)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (hnh util object)
  :use-module ((hnh util) :select (->)))

(define-type (f) x)

(test-group "Created procedures"
            (test-assert "Constructor"  (procedure? f))
            (test-assert "Predicate"    (procedure? f?))
            (test-assert "Field access" (procedure? x)))

;; (f)
;; (f x: 10)
;; (f? (f))

(test-equal "Accessors are getters"
  10 (x (f x: 10)))
(test-assert "Accessors update, returning a object of the original type"
  (f? (x (f x: 10) 20)))
(test-equal "A get after an update returns the new value"
  20 (-> (f x: 10)
         (x 20)
         x))


(define-type (g) x)

(test-assert "Second type can be created"
  (g x: 10))

(test-assert "Second type isn't first type"
  (not (f? (g x: 10))))

(test-assert "First type isn't second type"
  (not (g? (f x: 10))))

;; Tests that the old x gets shadowed
;; (test-equal 10 (x (f x: 10)))
;; (test-equal 10 (x (g x: 10)))

;; field-level arguments
;; - init:
(define-type (f2) (f2-x default: 0 type: integer?))
(test-equal 0 (f2-x (f2)))

;; - type:

(test-error "Giving an invalid type to the constructor throws an error"
            'wrong-type-arg (f2 f2-x: 'hello))
(test-error "Giving an invalid type to a setter throws an error"
            'wrong-type-arg (f2-x (f2) 'hello))
(test-equal "The error includes the name of the field, the expected type, and the given value"
  '(f2-x integer? hello)
  (catch 'wrong-type-arg (lambda () (f2-x (f2) 'hello))
    (lambda (err proc fmt args data) args)))

(test-equal "Typed setter updates the value"
            (f2 f2-x: 10) (f2-x (f2) 10))

;; type-level arguments
;; - constructor:
(define-type (f3 constructor: (lambda (make check)
                                (lambda* (#:key f3-x f3-y)
                                  (check f3-x f3-y)
                                  (make f3-x f3-y))))
  (f3-x type: integer?)
  (f3-y type: string?))

(test-assert "Custom constructors create objcets"
  (f3? (f3 f3-x: 10 f3-y: "Hello")))

(test-error "Bad arguments to custom constructor"
            'wrong-type-arg (f3 f3-x: 'hello f3-y: 'world))

;; - printer:
(define-type (f4 printer: (lambda (r p) (display "something" p))))
(test-equal "something" (with-output-to-string (lambda () (write (f4)))))

(test-group "Object equivalence"
  (let ((o1 (f3 f3-x: 10 f3-y: "string"))
        (o2 (f3 f3-x: 10 f3-y: "string")))
    (test-eq "An object is itself" o1 o1)
    (test-assert "Two identical objects are different objects"
      (not (eq? o1 o2)))
    (test-equal "Two identical objects are equal" o1 o2)))

'((hnh util object))
