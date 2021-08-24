(((sxml namespace)
  move-to-namespace
  ))


(test-equal "Move unnamespaced to namespace"
  '(NEW:test)
  (move-to-namespace '(test) '((#f . NEW))))

(test-equal "Swap namespaces"
  '(b:a (a:b))
  (move-to-namespace '(a:a (b:b)) '((a . b) (b . a))))

(test-equal "Remove all namespaces"
  '(a (b))
  (move-to-namespace '(a:a (b:b)) #f))

(test-equal "Move everything to one namespace"
  '(c:a (c:b))
  (move-to-namespace '(a:a (b:b)) 'c))

(test-equal "Partial namespace change"
  '(c:a (b:b))
  (move-to-namespace '(a:a (b:b))
                     '((a . c))))

(test-equal "Remove specific namespace"
  '(a:a (b))
  (move-to-namespace '(a:a (b:b))
                     '((b . #f))))
