(define-module (test html caltable)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (calp translation)

  :use-module (calp html components)
  )

(test-equal
    '(button (@ (class "btn") (onclick "onclick")) "Body")
  (btn onclick: "onclick" "Body"))

(test-equal "href button, without body"
    '(a (@ (class "btn") (href "href")))
  (btn href: "href"))

(test-error 'wrong-type-arg
  (btn href: "a" onclick: "b"))

(test-equal "btn no specifier, but class"
  '(button (@ (class "btn test")) "body")
  (btn class: '("test") "body"))

;; tabset

(test-equal '(link (@ (type "text/css") (rel "stylesheet") (href "style.css")))
  (include-css "style.css"))

(test-equal
    '(link (@ (type "text/css") (rel "stylesheet") (href "style.css") (class "test")))
  (include-css "style.css" '(class "test")))

(test-equal
    '(link (@ (type "text/css") (rel "alternate stylesheet") (href "style.css")))
  (include-alt-css "style.css"))
