(define-module (calp webdav resource file)
  :use-module (srfi srfi-1)
  :use-module (oop goops)
  :use-module (hnh util)
  :use-module (hnh util env)
  :use-module (hnh util path)
  :use-module (hnh util type)
  :use-module (datetime)
  :use-module (ice-9 popen)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 ftw)
  :use-module (ice-9 regex)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (calp webdav resource)
  :use-module (calp webdav property)
  :use-module (calp namespaces)
  :use-module (rnrs io ports)
  :use-module (rnrs bytevectors)
  :use-module (xattr)
  :use-module (calp util config)
  :export (<file-resource> file-resource? root ; path
                           make-resource
                           ))
;; NOTE:
;; Webdav makes no mention of symlinks,
;; Apache chooses to hide all symlinks [1].
;; The approach taken here is to treat all symlinks as their own
;; content type, with their content being their destination.
;; [1]: http://www.webdav.org/mod_dav/faq/#04-02

(define-config xattr-property-prefix "user.webdav"
  pre: (ensure string?))

;;; Resources backed by the filesystem
(define-class <file-resource> (<resource>)
  ;; Directory to act as root for this file tree.
  ;; Should be inherited by all children

  ;; DO NOT export the setters. These fields needs to be carefully managed to
  ;; ensure that they stay consistant with the @var{name} trail.
  (root getter: root setter: set-root! init-value: "/" init-keyword: root:)
  (path getter: path setter: set-path! init-value: "/" init-keyword: path:))

(define-method (initialize (self <file-resource>) args)
  (next-method)
  (typecheck (root self) string? "<file-resource>.root")
  (typecheck (path self) string? "<file-resource>.path"))

(define-method (write (self <file-resource>) port)
  (display
   (format #f "#<<file-resource> name=~s, root=~s, path=~s>"
           (name self)
           (root self)
           (path self))
   port))

(define (make-resource name . args)
  (apply make <file-resource> name: name args))

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
    (map (lambda (p) (file-resource-for-path
                 (root self)
                 (path-append (path self) p)))
         (remove (lambda (p) (member p '("." "..")))
                 ;; NOTE scandir returns #f on errors (such as permission denied).
                 ;; A better error message should possibly be used.
                 (or (scandir (filepath self))
                     '())))))

(define-method (is-collection? (self <file-resource>))
  (eq? 'directory (stat:type (lstat (filepath self)))))

(define (file-creation-date path)
  (-> path lstat stat:ctime unix-time->datetime))

(define (mimetype path)
  (let ((pipe (open-pipe* OPEN_READ "file" "--brief" "--mime-type"
                          path)))
    (begin1 (read-line pipe)
            (close-pipe pipe))))

(define-method (creationdate (self <file-resource>))
  (propstat 200
            (list ((xml webdav 'creationdate)
                   (with-locale1
                    LC_TIME "C"
                    (lambda ()
                      (-> (file-creation-date (filepath self))
                          (datetime->string "~Y-~m-~dT~H:~M:~S~Z"))))))))

(define-method (content (self <file-resource>))
  (case (stat:type (lstat (filepath self)))
    ((regular) (call-with-input-file (filepath self)
              get-bytevector-all binary: #t))
    ((symlink) (readlink (filepath self)))
    ((directory block-special char-special fifo socket unknown) #f)))

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


(define-method (setup-new-resource! (self <file-resource>)
                                    (parent <file-resource>))
  (next-method)
  (set-root! self (root parent))
  (set-path! self (path-append (path parent) (name self))))

(define-method (setup-new-collection! (self <file-resource>)
                                      (parent <file-resource>))
  (next-method)
  (mkdir (filepath self)))

(define-method (cleanup-resource (self <file-resource>))
  ((if (is-collection? self)
       rmdir
       delete-file)
   (filepath self)))


(define-method (content-length (self <file-resource>))
  (-> (filepath self) lstat stat:size))


(define-method (getcontenttype (self <file-resource>))
  ;; TODO 404 if collection
  ;; Or just omit it?
  (propstat 200 (list ((xml webdav 'getcontenttype)
                       (mimetype (filepath self))))))

(define-method (getlastmodified (self <file-resource>))
  (propstat 200
            (list ((xml webdav 'getlastmodified)
                   (with-locale1
                    LC_TIME "C"
                    (lambda ()
                      (-> (filepath self)
                          lstat
                          stat:mtime
                          unix-time->datetime
                          (datetime->string "~a, ~d ~b ~Y ~H:~M:~S GMT"))))))))

(define-method (set-dead-property!! (self <file-resource>) value)
  (typecheck value xml-element?)

  (lambda ()
    (set-xattr! (filepath self)
                (format #f "~a.~a"
                        (xattr-property-prefix)
                        (xml-element-hash-key value))
                (string->utf8
                 (with-output-to-string
                   (lambda () (namespaced-sxml->xml value))))
                follow-symlinks?: #f)))


(define-method (get-dead-property (self <file-resource>) value)
  (cond
   ((get-xattr (filepath self)
               (format #f "~a.~a"
                       (xattr-property-prefix)
                       (xml-element-hash-key value))
               follow-symlinks?: #f)
    => (lambda (bv)
         ;; TODO xml->namespaced-sxml may crash, if another program
         ;; has modified the xattr to contain invalid xml.
         ;; TODO similarly, we should verify that the element we get back
         ;; matches the key
         (propstat 200 (list (-> bv utf8->string xml->namespaced-sxml
                                 xml-document-root)))))
   (else (propstat 404 (list value)))))

(define-method (dead-properties (self <file-resource>))
  (filter-map (lambda (name)
                (cond ((string-match (format #f "^~a[.](.+):([^:]+)$"
                                             (regexp-quote (xattr-property-prefix)))
                                     name)
                       => (lambda (m) ((if (string=? "#f" (match:substring m 1))
                                      (xml (string->symbol (match:substring m 2)))
                                      (xml (string->symbol (match:substring m 1))
                                           (string->symbol (match:substring m 2)))))))
                      (else #f)))
              (list-xattr (filepath self) follow-symlinks?: #f)))


(define-method (remove-dead-property!! (self <file-resource>) value)
  (typecheck value xml-element?)
  (lambda ()
    (remove-xattr!
     (filepath self)
     (format #f "~a.~a" (xattr-property-prefix) (xml-element-hash-key value)))))
