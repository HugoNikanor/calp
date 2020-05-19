(((util) filter-sorted))

(test-equal "Filter sorted"
  '(3 4 5)
  (filter-sorted (lambda (x) (<= 3 x 5)) (iota 10)))
