#!/usr/bin/guile \
-s
!#

;;; Commentary:
;; Bulids an index of "all" locations at LiU, and prints it as a JSON
;; object on the form { "location name": "url-fragment", ... }. These
;; fragments should be appended to the base "https://old.liu.se/karta/".
;;
;; See static/user/user-additions.js for this script in action.
;;; Code:

(use-modules (srfi srfi-1)
             (web client)
             (web response)
             (ice-9 rdelim)
             (ice-9 format)
             (sxml gumbo)
             (sxml match)
             (json))

(define-values (response body) (http-get "https://old.liu.se/karta/list?l=sv"))

(unless (= 200 (response-code response))
  (format #t "Fetching index failed with ~a ~a~%"
          (response-code response)
          (response-reason-phrase response))
  (format #t "~{~s~%~}" (response-headers response))
  (exit 1))

(define data (html->sxml body))

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

(scm->json (hash-map->list (lambda (name frag)
                             `(,name . ,frag))
                           link-table))
(newline)
