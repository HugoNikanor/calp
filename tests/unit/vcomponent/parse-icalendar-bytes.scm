(define-module (test parse-icalendar-bytes)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (vcomponent media-type text calendar parse-structure)
  :use-module (vcomponent media-type text calendar parse-types)
  )

(test-equal "UTF-8 entry broken by line folding CRNL version"
  (list (logical-line content: "ö" line: 1))
  (bytevector->unfolded-lines
   #vu8(#xc3                 ; first half of utf-8 two byte character
        #x0d #x0a #x20       ; newline, and continuation marker
        #xb6                 ; second half of utf-8 two byte character
        )))


(test-equal "UTF-8 entry broken by line folding NL version"
  (list (logical-line content: "ö" line: 1))
  (bytevector->unfolded-lines
   #vu8(#xc3                 ; first half of utf-8 two byte character
        #x0a #x20            ; newline, and continuation marker
        #xb6                 ; second half of utf-8 two byte character
        )))

'((vcomponent media-type text calendar parse-structure))
