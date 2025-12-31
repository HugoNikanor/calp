(define-module (test set)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util optional)
  :use-module (hnh util set))

;;; data-set
;;; set?

;;; set-empty?
(test-group "set-empty?"
  (test-assert "empty" (set-empty? (data-set)))
  (test-assert "not empty"
    (not
     (set-empty? (set-add (data-set) 'a)))))

;;; TODO list->set

(test-equal "set-add and set->list"
  '(1 2 3 4)
  (sort* (set->list (set-add (data-set) 1 2 3 4))
         <))

(test-group "set-pop"
  (test-group "pop any (empty set)"
    (let ((el set (set-pop (data-set) (nothing))))
      (test-equal (nothing) el)
      (test-assert (set-empty? set))))

  (test-group "pop any (non-empty set)"
    (let ((el set (set-pop (set-add (data-set) 1 2) (nothing))))
      ;; Indirect matches, since we don't know which
      ;; element was popped.
      (test-assert (just? el))
      (test-equal 1 (length (set->list set)))))

  (test-group "pop explicit, present"
    (let ((el set (set-pop (set-add (data-set) 1 2) (just 2))))
      (test-equal (just 2) el)
      (test-equal '(1) (set->list set))))

  (test-group "pop explicit, absent"
    (let ((el set (set-pop (set-add (data-set) 1 2) (just 3))))
      (test-equal (nothing) el)
      (test-equal '(1 2) (sort* (set->list set) <)))))

;;; These also test "complex" values in the set

(test-equal "set-intersection"
    (list (just 2))
  (set->list
   (set-intersection (list->set (list (just 1) (just 2)))
                     (list->set (list (just 2) (just 3))))))

(test-equal "set-union"
  (list (just 1) (just 2) (just 3))
  (sort*
   (set->list
    (set-union (list->set (list (just 1) (just 2)))
               (list->set (list (just 2) (just 3)))))
   < from-just))

(test-equal "set-difference"
  (list (just 1))
  (set->list
   (set-difference (list->set (list (just 1) (just 2)))
                   (list->set (list (just 2) (just 3))))))

'((hnh util set))
