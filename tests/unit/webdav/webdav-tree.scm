(define-module (test webdav-tree)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp webdav resource)
  :use-module (calp webdav resource virtual)
  :use-module (calp webdav resource file)
  :use-module (oop goops)
  :use-module (rnrs io ports)
  :use-module ((scheme base) :select (string->utf8))
  :use-module ((hnh util) :select (sort*))
  :use-module (hnh util path)
  )

(define dir (mkdtemp (string-copy "/tmp/webdav-tree-XXXXXX")))
(with-output-to-file (path-append dir "greeting")
  (lambda () (display "Hello, World!\n")))

(define root-resource (make <virtual-resource>))

(define virtual-resource (make <virtual-resource> content: (string->utf8 "I'm Virtual!")))

(define file-tree (make <file-resource> path: dir))

(mount-resource! file-tree root-resource "files")
(mount-resource! virtual-resource root-resource "virtual")

(test-equal "All resources in tree, along with href items"
    (list (cons '() root-resource)
          (cons '("files") file-tree)
          ;; TODO this can't work, since file doesn't return stable resource objects
          (cons '("files" "greeting") (cdr (car (children file-tree))))
          (cons '("virtual") virtual-resource))
  (sort* (all-resources-under root-resource) string< (compose string-concatenate car)))



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
