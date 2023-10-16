(define-module (test table)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (->))
  :use-module (hnh util table))

(test-expect-fail "Equivalent tables are equal")

(define t (table))

(test-assert "Empty tables are empty" (null? (table->list t)))

(test-equal "Adding elements work"
  '((a . 10) (b . 20))
  (-> t
      (table-put 'a 10)
      (table-put 'b 20)
      table->list))

(test-assert "Original table is unchanged"
  (null? (table->list t)))

(test-equal "Insertion order doesn't matter, when serializing to lists"
  (-> t
      (table-put 'a 10)
      (table-put 'b 20)
      table->list)
  (-> t
      (table-put 'b 20)
      (table-put 'a 10)
      table->list))

;;; TODO this doesn't work, since the trees don't rebalance
(test-equal "Equivalent tables are equal"
  (-> t
      (table-put 'a 10)
      (table-put 'b 20)
      (table-put 'c 30))
  (-> t
      (table-put 'c 30)
      (table-put 'b 20)
      (table-put 'a 10)))

;;; TODO test table-get
;; (table-get t 'key)
;; (table-put t 'key value)

'((hnh util table))
