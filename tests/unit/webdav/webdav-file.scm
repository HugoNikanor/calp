(define-module (test webdav-file)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (ice-9 ftw)
  :use-module (ice-9 rdelim)
  :use-module (oop goops)
  :use-module (calp webdav resource)
  :use-module (calp webdav resource file)
  :use-module ((scheme base) :select (string->utf8))
  )

;;; Commentary:
;;; Tests the specifics of the file backed webdav resource objects.
;;; Code:


(define test-root (mkdtemp (string-copy "/tmp/calp-test-XXXXXX")))

(define root-resource (make <file-resource> path: test-root))


(test-group "File resource collection"
  (create-collection! root-resource "subdir")
  (test-eqv "Collection correctly added"
    'directory (-> (path-append test-root "subdir")
                   stat stat:type) ))



(test-group "File resource with content"
  (let ((fname "file.txt")
        (s "Hello, World!\n"))
    (define file-resource
      (create-resource! root-resource fname
                        '() (string->utf8 s)))
    (let ((p (path-append test-root fname)))
      (test-eqv "File correctly added"
        'regular (-> p stat stat:type))
      (test-equal "Expected content was written"
        s
        (with-input-from-file p
          (lambda () (read-delimited "")))
        ))))



(test-group "Copy file"
  'TODO)

'((calp webdav resource)
  (calp webdav resource file))
