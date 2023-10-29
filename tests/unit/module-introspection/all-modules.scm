(define-module (test module-introspection all-modules)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (sort*))
  :use-module (hnh module-introspection all-modules))


(test-group "fs-find"
  (for-each (lambda (reference entry)
              (test-equal "Filename" (list-ref reference 1) (list-ref entry 0))
              (test-group "Stat data"
                (test-assert (vector? (list-ref entry 1)))
                (test-equal 18 (vector-length (list-ref entry 1))))
              (test-equal "File type" (list-ref reference 0) (list-ref entry 2))
              )
            '((directory "tests/test-module-tree")
              (regular "tests/test-module-tree/README.md")
              (directory "tests/test-module-tree/a")
              (regular "tests/test-module-tree/a.scm")
              (regular "tests/test-module-tree/a/b.scm")
              (regular "tests/test-module-tree/a/c.scm")
              (regular "tests/test-module-tree/b.scm"))
            (sort*
             (fs-find "tests/test-module-tree")
             string< car)))



(test-equal "all-files"
  '("tests/test-module-tree/README.md"
    "tests/test-module-tree/a.scm"
    "tests/test-module-tree/a/b.scm"
    "tests/test-module-tree/a/c.scm"
    "tests/test-module-tree/b.scm")
  (sort*
   (all-files-under-directory "tests/test-module-tree"
                              "")
   string<))

(test-equal "all SCM files"
  '("tests/test-module-tree/a.scm"
    "tests/test-module-tree/a/b.scm"
    "tests/test-module-tree/a/c.scm"
    "tests/test-module-tree/b.scm")
  (sort*
   (all-files-under-directory "tests/test-module-tree"
                              ".scm")
   string<))


;; module-file-mapping
;; all-files-and-modules-under-directory

(test-equal "all SCM files and modules"
  '(("tests/test-module-tree/a.scm" (a))
    ("tests/test-module-tree/a/b.scm" (a b))
    ("tests/test-module-tree/a/c.scm" #f)
    ("tests/test-module-tree/b.scm" (b)))
  (sort*
   (all-files-and-modules-under-directory "tests/test-module-tree")
   string< car))

(test-group "all-modules-under-directory"
  (let ((files modules
               (all-modules-under-directory
                "tests/test-module-tree")))
    (test-equal "Files"
      '("tests/test-module-tree/a.scm"
        "tests/test-module-tree/a/b.scm"
        "tests/test-module-tree/a/c.scm"
        "tests/test-module-tree/b.scm")
      (sort* files string<))
    (test-equal "Modules"
      '((a) (b) (a b))
      (sort modules
            (lambda (a b)
              (cond ((< (length a) (length b)) #t)
                    ((> (length a) (length b)) #f)
                    (else (string<?
                           (string-concatenate (map symbol->string a))
                           (string-concatenate (map symbol->string b))))))))))
(test-equal "Files"
  '(((a)   . "tests/test-module-tree/a.scm")
    ((a b) . "tests/test-module-tree/a/b.scm")
    ((b)   . "tests/test-module-tree/b.scm"))
  (sort*
   (module-file-mapping "tests/test-module-tree")
   string< cdr))

'((hnh module-introspection all-modules))

