(((calp util) filter-sorted set/r!))

(test-equal "Filter sorted"
  '(3 4 5)
  (filter-sorted (lambda (x) (<= 3 x 5)) (iota 10)))

(test-equal "set/r! single"
  #f
  (let ((x #t))
    (set/r! x = not)))
