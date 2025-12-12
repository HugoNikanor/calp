(define-module (test module-introspection module-uses)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (sort*))
  :use-module (hnh module-introspection module-uses))


(test-equal "Basic example"
    '(((srfi srfi-1) hide: () autoload: #f)
      ((not-a-real-module) select: (x) hide: () autoload: #f))
 (module-uses*
  '((define-module (test-module)
      :use-module (srfi srfi-1))

    (@ (not-a-real-module) x))))

(test-equal "Multiple imports of same module"
    '(((xdg basedir) hide: () autoload: #f)
      ((xdg basedir) select: (config-home) hide: () autoload: #f))
    (module-uses*
     '((use-modules
        ((xdg basedir)))
       (@ (xdg basedir) config-home))))

(test-equal "Multiple imports of same thing + alternative keyword"
    '(((xdg basedir) hide: () prefix: xdg- autoload: #f)
      ((xdg basedir) select: (config-home) hide: () autoload: #f))
    (module-uses*
     '((define-module (test)
         ;; Prefix written as keyword, to test that normalization
         :use-module ((xdg basedir) #:prefix xdg-))
       (@ (xdg basedir) config-home))))

(test-equal "Autoload + import private"
  '(((mod-1) select: (x) hide: () autoload: #t)
    ((mod-2) select: (z) hide: () autoload: #f))
  (module-uses*
   '((define-module (test)
       :autoload (mod-1) (x))
     (@@ (mod-2) z))))

(test-equal ":version on define-module, along with \"weird\" symbols"
  '(((tt) select: (y) hide: () version: (0 1) autoload: #f))
  (module-uses*
   '((define-module (test)
       :version (1 1)
       :use-module ((tt) :select (y) :version (0 1))))))

(test-error 'wrong-type-arg
  (module-uses*
   '((use-modules x))))

(test-error 'wrong-type-arg
  (module-uses*
   '((use-modules (x :select (y))))))

(test-error 'wrong-type-arg
  (module-uses*
   '((use-modules ((x) (y))))))

(test-error 'wrong-type-arg
  (module-uses*
   '((use-modules ((x) 1 (y))))))

'((hnh module-introspection module-uses))
