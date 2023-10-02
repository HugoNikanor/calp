(define-module (test webdav-util)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp webdav resource base))

(test-group "string->href"
  (test-equal "Root path becomes null"
    '() (string->href "/"))
  (test-equal "Trailing slashes are ignored"
    '("a" "b") (string->href "/a/b/")))

(test-group "href->string"
  (test-equal "Null case becomes root path"
    "/" (href->string '()))
  (test-equal "Trailing slashes are not added"
    "/a/b" (href->string '("a" "b"))))

(test-group "href-relative"
  (test-equal '("a" "b") (href-relative '() '("a" "b")))
  (test-equal '("b") (href-relative '("a") '("a" "b")))
  (test-equal '() (href-relative '("a" "b") '("a" "b")))

  (test-error 'misc-error
    (href-relative '("c") '("a" "b")))

  (test-error 'misc-error
    (href-relative '("c") '())))

'((calp webdav resource base))
