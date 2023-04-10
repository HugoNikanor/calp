(define-module (calp webdav proppatch)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp webdav property)
  :use-module (calp webdav resource)
  :use-module (sxml match)
  :use-module (sxml namespaced)
  :use-module ((hnh util) :select (for))
  :export (parse-propertyupdate)
  )


(define (parse-propertyupdate body namespaces resource)
  (merge-propstats
   (sxml-match body
     [(d:propertyupdate . ,changes)
      (define continuations
        (concatenate
         (for change in changes
              (sxml-match change
                [(d:remove (d:prop . ,properties))
                 (map (lambda (prop) (cons prop
                                      (remove-property
                                       resource
                                       (car
                                        (sxml->namespaced-sxml prop namespaces)))))
                      properties)]

                ;; TODO handle xmllang correctly
                [(d:set (d:prop . ,properties))
                 (map (lambda (prop) (cons prop
                                      (set-property resource
                                                    (sxml->namespaced-sxml prop namespaces))))
                      properties)]

                [,else (scm-error 'bad-request ""
                                  "Invalid propertyupdate: ~s"
                                  (list body)
                                  (list body))]))))

      ;; (format (current-error-port) "~s~%" continuations)
      (let loop ((continuations continuations))
        (if (null? continuations)
            '()
            (let ((tag proc (car+cdr (car continuations))))
              (set! tag (sxml->namespaced-sxml tag namespaces))
              ;; (format (current-error-port) "tag: ~s~%" tag)
              (catch #t (lambda ()
                          ;; This is expected to throw quite often
                          (proc)
                          (cons (propstat 200 (list tag))
                                (loop (cdr continuations))))
                (lambda err
                  (cons (propstat 409 (list tag))
                        (mark-remaining-as-failed-dependency (cdr continuations))))))))]

     [,else (scm-error 'bad-request ""
                       "Invalid root element: ~s"
                       (list else)
                       (list else))])))


(define (mark-remaining-as-failed-dependency pairs)
  (map (lambda (item)
         (propstat 424 (list (car item))))
       pairs))
