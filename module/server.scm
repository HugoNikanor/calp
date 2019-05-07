(define-module (server)
  #:use-module (util))

(use-modules* (web (server request response uri))
              (output (html))
              (ice-9 (match control rdelim curried-definitions))
              (srfi (srfi-1 srfi-19)))

(define month-names
  '(jan feb mar apr may jun jul aug sep oct nov dec))

(define month-numbers
  (map cons month-names (iota 12 1)))

(define (query->alist str)
  (map (lambda (s) (let* (((k . v) (string-split s #\=)))
                (cons (string->symbol (string-downcase k)) v)))
       (filter (negate string-null?) (string-split str #\&))))


(define ((serve-file continuation) path-list)
  (continuation
   '((content-type text/css))
   ;; TODO document root somewhere around here
   (call-with-input-file (string-append "./" (string-join path-list "/"))
     read-string)))

;;; TODO "/static/*"
(define (make-handler calendars events)
  (lambda (request request-body)
    (format (current-error-port) "[~a] ~a~%"
            (date->string (current-date) "~3")
            request)
    (call/ec
     (lambda (ret)
       (let* ((uri (request-uri request)))
         (match uri
           (($ (@@ (web uri) <uri>) scheme userinfo host port path query fragment)
            (let* ((path-parts (string-split path #\/)))
              ;; This is actually quite ugly
              (and=> (member "static" path-parts) (serve-file ret))

              (let* ((query (query->alist (or query "")))
                     (m (and=> (assoc-ref query 'm)
                               (compose (lambda (k) (assoc-ref month-numbers k)) string->symbol car))))
                (when m
                 (let* ((start (make-date 0 0 0 0 1 m 2019 0))
                        (end (make-date 0 0 0 0 1 (1+ m) 2019 0))
                        (str (with-output-to-string (lambda () (html-generate calendars events start end)))))
                   (ret `((content-type text/html)) str)))
                (ret (build-response #:code 404)
                     "404 Not Fonud"))))))))))

(define-public (server-main c e args)
  (run-server (make-handler c e)))
