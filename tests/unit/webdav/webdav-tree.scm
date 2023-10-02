(define-module (test webdav-tree)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp webdav resource)
  :use-module (calp webdav resource virtual)
  :use-module (calp webdav resource file)
  :use-module (oop goops)
  :use-module (rnrs bytevectors)
  :use-module (rnrs io ports)
  :use-module ((hnh util) :select (sort*))
  :use-module (hnh util path)
  )

(define* (pretty-print-tree tree
                            optional: (formatter (lambda (el) (write el) (newline)))
                            key: (depth 0))
  (cond ((null? tree) 'noop)
        ((pair? tree)
         (display (make-string (* depth 2) #\space)) (formatter (car tree))
         (for-each (lambda (el) (pretty-print-tree el formatter depth: (+ depth 1)))
                   (cdr tree)))
        (else (formatter tree))))

(define-method (resource-tree (self <resource>))
  (cons self
        (map resource-tree (children self))))



(define dir (mkdtemp (string-copy "/tmp/webdav-tree-XXXXXX")))
(with-output-to-file (path-append dir "greeting")
  (lambda () (display "Hello, World!\n")))

(define root-resource (make <virtual-resource>
                        name: "*root*"))

(define virtual-resource (make <virtual-resource>
                           name: "virtual"
                           content: (string->bytevector "I'm Virtual!" (native-transcoder))))

(define file-tree (make <file-resource>
                    root: dir
                    name: "files"))

(mount-resource! root-resource file-tree)
(mount-resource! root-resource virtual-resource)

(test-equal "All resources in tree, along with href items"
    (list (cons '() root-resource)
          (cons '("files") file-tree)
          (cons '("files" "greeting") (car (children file-tree)))
          (cons '("virtual") virtual-resource))
  (sort* (all-resources-under root-resource) string< (compose string-concatenate car)))



;; (pretty-print-tree (resource-tree root-resource))



;; (test-equal '("") (href root-resource) )                 ; /
;; ;; (test-equal '("" "virtual") (href virtual-resource))     ; /virtual & /virtual/
;; (test-equal '("virtual") (href virtual-resource))  ; /virtual & /virtual/
;; ;; (test-equal '("" "files") (href file-tree))              ; /files & /files/
;; (test-equal '("files") (href file-tree))           ; /files & /files/

(test-eqv "Correct amount of children are mounted"
  2 (length (children root-resource)))

(test-eq "Lookup root"
  root-resource (lookup-resource root-resource '()))

(test-eq "Lookup of mount works (virtual)"
  virtual-resource (lookup-resource root-resource '("virtual")))
(test-eq "Lookup of mount works (files)"
  file-tree (lookup-resource root-resource '("files")))

;; (test-equal "File resource works as expected"
;;   "/home/hugo/tmp"
;;   (path file-tree))

(let ((resource (lookup-resource root-resource (string->href "/files/greeting"))))
  (test-assert (resource? resource))
  (test-assert (file-resource? resource))
  ;; (test-equal "/files/greeting" (href->string (href resource)))
  (test-equal "Hello, World!\n" (bytevector->string (content resource) (native-transcoder)))
 )

'((calp webdav resource)
  (calp webdav resource virtual)
  (calp webdav resource file))
