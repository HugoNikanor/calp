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
  )

;;; Commentary:
;;; Tests the specifics of the file backed webdav resource objects.
;;; Code:


;;; TODO general helper procedure for this
(define test-root (mkdtemp (string-copy "/tmp/calp-test-XXXXXX")))

(define root-resource (make <file-resource>
                        root: test-root))


(test-group "File resource collection"
  (add-collection! root-resource "subdir")
  (test-eqv "Collection correctly added"
    'directory (-> (path-append test-root "subdir")
                   stat stat:type) ))



;;; TODO this fails, sice <file-resource> doesn't override add-resource!
;;; <file-resources>'s add resource must at least update root path path of the
;;; child resource, and possibly also touch the file (so ctime gets set).
(test-group "File resource with content"
  (let ((fname "file.txt")
        (s "Hello, World!\n"))
    (add-resource! root-resource fname s)
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
