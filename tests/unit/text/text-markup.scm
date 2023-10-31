(define-module (test text-markup)
  :use-module (srfi srfi-64)
  :use-module (text markup))

(test-equal "Header"
    "    \x1b[1mHello\x1b[m    \n"
  (sxml->ansi-text
   '(header (@ (width 20))
            "Hello")))

(test-equal "Center"
    "     .     "
  (sxml->ansi-text
   '(center (@ (width 11)) ".")))

(test-equal "Bold is NOT restored after a nested italic"
    "\x1b[1mbold\x1b[3mitalic\x1b[mafter\x1b[m"
  (sxml->ansi-text '(b (group "bold" (i "italic") "after"))))

(test-equal "hr"
  "     ────────────────────────────────────────────────────────────     \n"
  (sxml->ansi-text '(hr)))

(test-group "Description list"
 (test-equal
     (string-append
      "  key │ value\n"
      "key 2 │ value 2\n")
   (sxml->ansi-text '(dl (dt "key")
                         (dd "value")
                         (dt "key 2")
                         (dd "value 2"))))

(test-equal "Muliple dt to one dd"
  (string-append
   "  key │ value\n"
   "key 2 │ \n"
   "  key │ value\n")
   (sxml->ansi-text '(dl (dt "key")
                         (dt "key 2")
                         (dd "value")
                         (dt "key")
                         (dd "value"))))

 (test-equal "Muliple dd to one dt"
   (string-append
    "  key │ value\n"
    "      │ value 2\n"
    "key 2 │ \n")
   (sxml->ansi-text '(dl (dt "key")
                         (dd "value")
                         (dd "value 2")
                         (dt "key 2")))))

(test-equal "Scheme block"
    "(sxml->ansi-text
  '(dl (dt \"key\")
       (dd \"value\")
       (dd \"value 2\")
       (dt \"key 2\")))"
  (sxml->ansi-text
   '(scheme (sxml->ansi-text '(dl (dt "key")
                                  (dd "value")
                                  (dd "value 2")
                                  (dt "key 2"))))))

(test-equal "Unknown tag"
  "\x1b[1m??`\x1b[7m(\"Hello, \" (b \"nested\") \"after\")\x1b[m'\n"
  (sxml->ansi-text '(block (err "Hello, " (b "nested") "after")
                           (br))))



(test-equal "Empty document"
  ""
  (sxml->ansi-text '()))

(test-equal "Paragraph text flowing + nested format"
  ;; This also shows bad spacing
  ;; second line is longer that the expected line width.
  "A    sentance   \x1b[1mcontaining\x1b[m    the   word
pneumonoultramicroscopicsilicovolcanoconiosis
is rather hard to justify properly

"
  (sxml->ansi-text
   '(p (@ (width 40)) "A sentance " (b "containing") " the word pneumonoultramicroscopicsilicovolcanoconiosis is rather hard to justify properly")))


(test-equal "blockquote"
  "    Hello
    -- Hugo
" (sxml->ansi-text '(blockquote "Hello" (br) "-- Hugo")))


'((text markup))
