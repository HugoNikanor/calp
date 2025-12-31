(define-module (test table)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (->))
  :use-module (hnh util table)
  :use-module (hnh util optional)
  :use-module (hnh util named-type))

(test-assert "Empty tables are empty" (null? (table->list (table))))

(test-equal "Adding elements work"      ; Also tests table->list
  '((a . 10) (b . 20))
  (-> (table)
      (table-put 'a 10)
      (table-put 'b 20)
      table->list))

(test-assert "Original table is unchanged"
  (null? (table->list (table))))

;;; TODO insertion order matters when serializing to lists.
;; (test-equal "Insertion order doesn't matter, when serializing to lists"
;;   (-> (table)
;;       (table-put 'a 10)
;;       (table-put 'b 20)
;;       table->list)
;;   (-> (table)
;;       (table-put 'b 20)
;;       (table-put 'a 10)
;;       table->list))

(test-assert "Equivalent tables are equal"
  (table-equal?
   (-> (table)
       (table-put 'a 10)
       (table-put 'b 20)
       (table-put 'c 30))
   (-> (table)
       (table-put 'c 30)
       (table-put 'b 20)
       (table-put 'a 10))))

;;; TODO test table-get
;; (table-get (table) 'key)
;; (table-put (table) 'key value)

;;; TODO test table-remove
;;; It seems completely broken currently
;; scheme@(guile-user) [3]> (table-remove $17 'TZID)
;; ice-9/boot-9.scm:1676:22: In procedure raise-exception:
;; In procedure fold: Wrong type argument: (table)

(test-assert "Remove on empty table leaves an empty table"
  (-> (table)
      (table-remove 'key)
      table->list
      null?))

(test-equal "Remove of present key removes that entry (but no others), A"
  (-> (table)
      (table-put 'a 1)
      (table-put 'b 2)
      (table-remove 'a)
      table->list)
  `((b . 2)))

(test-equal "Remove of present key removes that entry (but no others), B"
  (-> (table)
      (table-put 'a 1)
      (table-put 'b 2)
      (table-remove 'b)
      table->list)
  `((a . 1)))

(test-equal "Remove of non-existing key is a no-op"
  (-> (table)
      (table-put 'a 1)
      (table-remove 'b)
      table->list)
  `((a . 1)))

;;; TODO test table-focus
;;; TODO test serialize-table
;;; TODO test alist->table

(test-group "Typed tables"
  (define t (table (named-type string?)))
  (test-equal "Successfull insert" '((a . "Hello"))
    (table->list (table-put t 'a "Hello")))

  (test-error "Type error on insert"
    'wrong-type-arg (table-put t 'a 1))

  ;; TODO test that type persists when focusing deeper nodes, and when removing nodes
  )

(test-group "Table diff"
  (test-equal "No diff"
    '()
    (table-diff
     (alist->table '((a . 1) (b . 2)))
     (alist->table '((b . 2) (a . 1)))))

  (test-equal "Various diffs"
    '((absent b k1)
      (diff k2 "x" "y")
      (absent a k3))
    (table-diff (alist->table '((k1 . 1) (k2 . "x")))
                (alist->table '((k3 . 3) (k2 . "y"))))))

(test-group "table-preview"
  (let ((t (alist->table '((a . 1)))))
   (test-equal (just 1) (table-preview t 'a))
   (test-equal (nothing) (table-preview t 'b))))

(test-equal "table-filter-map"
  '()
  (table-diff
   (alist->table '((k2 . k2)))
   (table-filter-map
    (lambda (k v) (if (even? v) (just k) (nothing)))
    (alist->table '((k1 . 1) (k2 . 2))))))

(test-group "table-union"
  (test-equal
      '()
    (table-diff
     (alist->table '((a . 1)
                     (b . 3)
                     (c . 4)))
     (table-union
      (alist->table '((a . 1) (b . 2)))
      (alist->table '((b . 3) (c . 4)))))))

(test-group "table-intersection"
  (test-equal
      '()
    (table-diff
     (alist->table '((b . 2)))
     (table-intersection
      (alist->table '((a . 1) (b . 2)))
      (alist->table '((b . 3) (c . 4)))))))

(test-group "table-difference"
  (test-equal
      '()
    (table-diff
     (alist->table '((a . 1)))
     (table-difference
      (alist->table '((a . 1) (b . 2)))
      (alist->table '((b . 3) (c . 4)))))))

'((hnh util table))
