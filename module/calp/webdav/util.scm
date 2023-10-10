(define-module (calp webdav util)
  :use-module (srfi srfi-1)
  :use-module (ice-9 regex)
  :use-module (web uri)
  :export (parse-dav-line
           validate-dav-line
           write-dav-line)
  )

;;; Parse a DAV HTTP header's content as specified in RFC 4918 §10.1
;;; For example:
;;;     DAV: 1, 2, access-control, calendar-access
(define (parse-dav-line str)
  (map (lambda (item)
         (cond ((string-match "^[0-9]+$" item)
                => (lambda (m) (string->number (match:substring m))))
               ((string-match "^<(.*)>$" item)
                => (lambda (m) (string->uri (match:substring m 1))))
               (else (string->symbol item))))
       (map string-trim-both (string-split str #\,))))

(define (validate-dav-line lst)
  (every (lambda (item)
           (or (and (number? item) (<= 1 item 3))
               (uri? item)
               ;; Possibly check against list of valid tokens
               (symbol? item)))
         lst))



(define (write-dav-line lst port)
  (display
   (string-join (map (lambda (item)
                       (cond ((number? item) (number->string item))
                             ((uri? item) (string-append "<" (uri->string item) ">"))
                             (else (symbol->string item))))
                     lst)
                ", " 'infix)
   port))
