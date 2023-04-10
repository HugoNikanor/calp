(define-module (calp webdav resource file)
  :use-module (srfi srfi-1)
  :use-module (oop goops)
  :use-module (hnh util)
  :use-module (hnh util env)
  :use-module (hnh util path)
  :use-module (datetime)
  :use-module (ice-9 popen)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 ftw)
  :use-module (sxml namespaced)
  :use-module (calp webdav resource)
  :use-module (calp webdav property)
  :use-module (calp namespaces)
  :use-module (rnrs io ports)
  :use-module (rnrs bytevectors)
  :export (<file-resource> file-resource? root ; path
                           ))

;;; Resources backed by the filesystem
(define-class <file-resource> (<resource>)
  ;; Directory to act as root for this file tree.
  ;; Should be inherited by all children

  ;; DO NOT export the setters. These fields needs to be carefully managed to
  ;; ensure that they stay consistant with the @var{name} trail.
  (root getter: root setter: set-root! init-value: "/" init-keyword: root:)
  (path getter: path setter: set-path! init-value: "/" init-keyword: path:))

(define (file-resource? x)
  (is-a? x <file-resource>))

;; TODO this is global, so most certanly leaks info between different
;; <file-resource> trees.
(define *realized-resource* (make-hash-table))

(define (file-resource-for-path root path)
  (or (hash-ref *realized-resource* path)
      (let ((resource (make <file-resource>
                        ;; href:
                        root: root
                        ; local-path: path
                        name: (basename path)
                        path: path
                        )))
        (hash-set! *realized-resource* path resource)
        resource)))

(define (filepath self)
  (path-append (root self)
               (path self)))

(define-method (children (self <file-resource>))
  ;; (format (current-error-port) "root=~s, path=~s~%"
  ;;         (root self)
  ;;         (local-path self))
  (when (is-collection? self)
    (map (lambda (p) (file-resource-for-path (root self)
                                        (path-append (path self)
                                                     p)))
         (remove (lambda (p) (member p '("." "..")))
                 (scandir (filepath self))))))

(define-method (is-collection? (self <file-resource>))
  (eq? 'directory (stat:type (stat (filepath self)))))

(define (file-creation-date path)
  (let ((pipe (open-pipe* OPEN_READ "stat" "-c" "%W" path)))
    (begin1 (unix-time->datetime (read pipe))
            (close-pipe pipe))))

(define (mimetype path)
  (let ((pipe (open-pipe* OPEN_READ "file" "--brief" "--mime-type"
                          path)))
    (begin1 (read-line pipe)
            (close-pipe pipe))))

(define-method (creationdate (self <file-resource>))
  (propstat 200
            `((,(xml webdav 'creationdate)
               ,(with-locale1
                 LC_TIME "C"
                 (lambda ()
                  (-> (file-creation-date (filepath self))
                      (datetime->string "~Y-~m-~dT~H:~M:~S~Z"))))))))

(define-method (content (self <file-resource>))
  (if (is-collection? self)
      #f
      (call-with-input-file (filepath self)
        get-bytevector-all binary: #t)))

(define-method (set-content! (self <file-resource>) data)
  (cond ((bytevector? data)
         (call-with-output-file (filepath self)
           (lambda (port) (put-bytevector port data))))
        ((string? data)
         (call-with-output-file (filepath self)
           (lambda (port) (put-string port data))))
        (else (scm-error 'misc-error "set-content!<file-resource>"
                         "Content must be bytevector or string: ~s"
                         (list data) #f))))


;; This is currently ONLY called from add-resource! which creates a
;; child from the type of the parent.
(define-method (setup-new-resource! (self <file-resource>)
                                    (parent <file-resource>))
  (set-root! self (root parent))
  (set-path! self (path-append (path parent) (name self))))


(define-method (add-collection! (self <file-resource>) new-name)
  (let ((resource (next-method)))
    (set-root! resource (root self))
    (set-path! resource (path-append (path self) new-name))
    (mkdir (path-append (root resource) (path resource)))
    resource))

(define-method (cleanup-resource (self <file-resource>))
  ((if (is-collection? self)
       rmdir
       delete-file)
   (filepath self)))

(define-method (content-length (self <file-resource>))
  (-> (filepath self) stat stat:size))


(define-method (getcontenttype (self <file-resource>))
  ;; TODO 404 if collection
  ;; Or just omit it?
  (propstat 200 `((,(xml webdav 'getcontenttype)
                   ,(mimetype (filepath self))))))

(define-method (getlastmodified (self <file-resource>))
  (propstat 200
            `((,(xml webdav 'getlastmodified)
               ,(with-locale1
                 LC_TIME "C"
                 (lambda ()
                  (-> (filepath self)
                      stat
                      stat:mtime
                      unix-time->datetime
                      (datetime->string "~a, ~d ~b ~Y ~H:~M:~S GMT"))))))))

;; (define (xattr-key xml-el)
;;   (format #f "caldav.~a"
;;           (base64-encode
;;            (format #f "~a:~a"
;;                    (xml-element-namespace xml-el)
;;                    (xml-element-tagname xml-el)))))


;; (define-method (set-dead-property (self <file-resource>) value)
;;   (unless (and (list? value)
;;                (xml-element? (car value)))
;;     (scm-error 'misc-error "set-dead-property"
;;                "Invalid value, expected namespaced sxml"
;;                '() #f))
;;   (catch #t
;;     (lambda ()
;;       (lambda ()
;;         (xattr-set!
;;          (filename self)
;;          (xattr-key (car value))
;;          (with-output-to-string
;;            (lambda () (namespaced-sxml->xml value))))))
;;     (lambda _ (next-method))))


;; (define-method (get-dead-property (self <file-resource>)
;;                            xml-el)
;;   (catch #t
;;     (lambda ()
;;       (propstat 200
;;                 (list
;;                  (xattr-ref (filepath self)
;;                             (xattr-key el)))))
;;     (lambda _ (next-method))))


;; (define-method (remove-dead-property (self <file-resource>)
;;                               xml-el)
;;   (catch #t
;;     (lambda () (xattr-remove! (filepath self) xml-el))
;;     (lambda _ (next-method))))
