#!/usr/bin/guile \
-e main -s
!#

;;; Commentary:
;; Bulids an index of "all" locations at LiU, and prints it as a JSON
;; object on the form { "location name": "url-fragment", ... }. These
;; fragments should be appended to the base "https://old.liu.se/karta/".
;;
;; See static/user/user-additions.js for this script in action.
;;; Code:

(use-modules (srfi srfi-1)
             (srfi srfi-88)
             (web client)
             (web response)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 getopt-long)
             (sxml gumbo)
             (sxml match)
             ((hnh util) :select (->))
             (json))


;; Parse string as HTML, find all links which are "map links",
;; and return them as an association list from name to url-fragments.
(define (get-data string)
  (define data (html->sxml string))

  (define rx (make-regexp "^karta\\?"))

  (define links
    (map (lambda (node)
           (sxml-match node
                       [(a (@ (href ,href)) ,b0 ,body ...)
                        (cons href b0)]))
         (((@ (sxml xpath) sxpath) '(// a)) data)))

  (define map-links (filter (lambda (pair) (regexp-exec rx (car pair)))
                            links))

  (define link-table (make-hash-table))
  (for-each (lambda (pair) (hash-set! link-table (string-upcase (string-trim-both (cdr pair)))
                                      (car pair)))
            map-links)

  (hash-map->list (lambda (name frag)
                    `(,name . ,frag))
                  link-table))

;; Open a HTTP request to the given URL, and return the
;; response body as a port.
(define (open-input-url url)
  (define-values (response body) (http-get url streaming?: #t))

  (unless (= 200 (response-code response))
    (format #t "Fetching index failed with ~a ~a~%"
            (response-code response)
            (response-reason-phrase response))
    (throw 'misc-error "get-from-url" "~{~s~%~}" (response-headers response))
    (exit 1))
  body)


(define (display-help)
  (format #t "Usage: fetch-liu-map-index.scm [--url=url] [--file=file]~%")
  (newline)
  (for-each (lambda (line) (display line) (newline))
   ((@ (text flow) flow-text)
    "(Possibly) fetches, and parses Linköpings Universities list of locations. The return is a simple JSON-object where the keys are the location names, and the values are URL fragments. The --file flag exists since the TLS bindings in Guile are currently broken, and is mainly useful for taking input from a cURL pipe."))
  (newline))

(define option-spec
  '((url (value #t))
    (file (value #t))
    (help (single-char #\h))))

(define (main args)
  (define options (getopt-long args option-spec))

  (when (option-ref options 'help #f)
    (display-help)
    (exit 0))

  (let ((port
         (cond ((option-ref options 'url #f)  => open-input-url)
               ((and=> (option-ref options 'file #f) (lambda (s) (string=? s "-")))
                (current-input-port))
               ((option-ref options 'file #f) => open-input-file)
               (else (open-input-url "https://old.liu.se/karta/list?l=sv")))))
    (-> port read-string get-data scm->json)
    (newline)))
