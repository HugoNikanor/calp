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
             ((sxml xpath) :select (sxpath))
             ((hnh util) :select (-> ->>))
             (json))

;; Fallback to ensure we have HTTPS
(define http-get
 (catch 'gnutls-not-available
   (lambda ()
     ((@@ (web client) ensure-gnutls))
     (@ (web client) http-get))
   (lambda _
     (use-modules (ice-9 popen)
                  (web http))

     (lambda (url . _)
       (let ((pipe (open-pipe* OPEN_READ "curl" "--include" "--http1.1" url)))
         (let ((intro (string-split (read-line pipe) #\space)) ; HTTP/1.1 200 OK\r
               (headers (read-headers pipe)))
           (let ((response
                  (build-response
                   version:  (parse-http-version (list-ref intro 0))
                   code: (string->number (list-ref intro 1))
                   reason-phrase: (string-trim-right
                                   (string-join (drop intro 2) " " 'infix)
                                   char-whitespace?)
                   headers: headers
                   port: pipe
                   validate-headers?: #t)))
             (values response pipe))))))))

;; Parse string as HTML, find all links which are "map links",
;; and return them as an association list from name to url-fragments.
(define (extract-data string)
  (define rx (make-regexp "^karta\\?"))

  ;; for (let el of document.querySelectorAll('a[href*="karta?"]')) {
  ;; 	ret[el.textContent.trim().toUpperCase()] = el.href
  ;; }

  (->> (html->sxml string)
       ((sxpath '(// a)))
       (map (lambda (node)
              (sxml-match node
                          [(a (@ (href ,href)) ,b0 ,body ...)
                           (cons href b0)])))
       (filter (lambda (pair) (regexp-exec rx (car pair))))
       (map (lambda (pair) (cons (string-upcase (string-trim-both (cdr pair)))
                            (car pair))))))

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
               ((string=? "-" (option-ref options 'file ""))
                (current-input-port))
               ((option-ref options 'file #f) => open-input-file)
               (else (open-input-url "https://old.liu.se/karta/list?l=sv")))))
    (-> port read-string extract-data scm->json)
    (newline)))
