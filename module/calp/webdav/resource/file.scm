(define-module (calp webdav resource file)
  :use-module (srfi srfi-1)
  :use-module (oop goops)
  :use-module (hnh util)
  :use-module (hnh util env)
  :use-module (hnh util path)
  :use-module (hnh util type)
  :use-module (datetime)
  :use-module ((ice-9 ftw) :select (scandir))
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
  :use-module (calp webdav resource file utils)
  :export (<file-resource> file-resource? path
                           make-resource
                           ))
;; NOTE:
;; Webdav makes no mention of symlinks,
;; Apache chooses to hide all symlinks [1].
;; The approach taken here is to treat all symlinks as their own
;; content type, with their content being their destination.
;; [1]: http://www.webdav.org/mod_dav/faq/#04-02

(define-config xattr-prefix "user.calp"
  pre: (ensure string?))

(define-config xattr-property-prefix
  ;; TODO is this evaluated at time of use, or time of define?
  ;; Its only useful if it's at time of use.
  (string-append (xattr-prefix) ".webdav")
  pre: (ensure string?))

;;; Resources backed by the filesystem
(define-class <file-resource> (<resource>)
  ;; Directory to act as root for this file tree.
  ;; Should be inherited by all children

  ;; DO NOT export the setters. These fields needs to be carefully managed to
  ;; ensure that they stay consistant with the @var{name} trail.
  ;; (root getter: root setter: set-root! init-value: "/" init-keyword: root:)
  (path getter: path setter: set-path! init-value: "/" init-keyword: path:))

(define-method (initialize (self <file-resource>) args)
  (next-method)
  ;; (typecheck (root self) string? "<file-resource>.root")
  (typecheck (path self) string? "<file-resource>.path"))

(define-method (write (self <file-resource>) port)
  (display
   (format #f "#<<file-resource> path=~s>"
           (path self))
   port))

(define-method (equal? (a <file-resource>) (b <file-resource>))
  (and (next-method)
       (string=? (path a) (path b))))

(define (make-resource . args)
  (apply make <file-resource> args))

(define (file-resource? x)
  (is-a? x <file-resource>))


(define-method (children (self <file-resource>))
  (if (collection? self)
      (map (lambda (p) (cons p
                        (make <file-resource>
                          path: (path-append (path self) p)
                          parent: self)))
           ;; NOTE scandir returns #f on errors (such as permission denied).
           ;; A better error message should possibly be used.
           (drop (or (scandir (path self))
                     '("." ".."))
                 2))
      '()))

(define-method (collection? (self <file-resource>))
  (directory? (path self)))

(define-method (creation-date (self <file-resource>))
  (file-creation-date (path self)))

;; Returns the path of the current resource, relative to the mount point,
;; For example, if the root file resource has "/home/example" as it's path,
;; and the resource references "/home/example/code/calp", then
;; "/code/calp" will be returned.
(define (filepath resource)
  (define file-root
    (let loop ((resource resource))
      (let ((p (parent resource)))
        (if (and p (file-resource? p))
            (loop p)
            resource))))
  (string-drop (path resource) (string-length (path file-root))))

;; A simple HTML directory listing.
;; Note that all links break if the directory is accessed without a trailing slash
(define-method (directory-listing (resource <resource>))
  (string->utf8
   (with-output-to-string
     (lambda ()
       (display "<!doctype html>") (newline)
       ((@ (sxml html) sxml->html)
        `(html (head (meta (@ (charset "UTF-8")))
                     (title "Directory listing"))
               (body
                (h1 "Directory listing for "
                    ;; NOTE this doesn't include where the root file resource is mounted
                    ,(filepath resource))
                (ul
                 ,@(when (and (parent resource) (file-resource? (parent resource)))
                     `((li (a (@ (href "..")) "↩️ Parent"))))
                 ,@(map (lambda (pair)
                          `(li (a (@ (href ,(car pair)
                                           ,(if (eq? 'directory (stat:type (lstat (path (cdr pair)))))
                                                "/" "")))
                                  ,(case (stat:type (lstat (path (cdr pair))))
                                     [(directory) "📁"]
                                     [(regular) "📰"]
                                     [(symlink) "🔗"]
                                     [(block-special) "🖴"]
                                     [(char-special) "🔌"]
                                     ;; [(fifo)]
                                     ;; [(socket)]
                                     [else "🙃"])
                                  " "
                                  ,(or (display-name (cdr pair))
                                       (car pair)))))
                        (children resource))))))))))

(define-method (content (self <file-resource>) headers)
  (case (stat:type (lstat (path self)))
    ((regular) (call-with-input-file (path self)
                 get-bytevector-all binary: #t))
    ((symlink) (readlink (path self)))
    ((directory)
     (cond ((get-child-by-name! self "index.html")
            (lambda (r) (and r (not (collection? r))))
            => (lambda (v) (content v headers)))
           (else (directory-listing self))))
    (else => (lambda (type)
               (throw 'http 403 (format #f "Can't access content of ~s files" type))))))

(define-method (set-content! (self <file-resource>) data headers)
  (cond ((collection? self)
         ;; "Method Not Allowed", since filesystems usually lacks the
         ;; ability to store data in the directory file. It's also
         ;; extra needed since we support "virtual" content on GET for
         ;; collections.
         (throw 'http 405))
   ((bytevector? data)
    (call-with-output-file (path self)
      (lambda (port) (put-bytevector port data)))
    #f)
        ((string? data)
         (call-with-output-file (path self)
           (lambda (port) (put-string port data)))
         #f)

        (else (throw 'http 400 (format #f "Content must be a bytevector or string, got: ~s" data)))))



(define-method (content-length (self <file-resource>))
  (case (stat:type (lstat (path self)))
    ((regular) (-> (path self) lstat stat:size))
    ((directory)
     (cond ((get-child-by-name! self "index.html")
            (lambda (r) (and r (not (collection? r)))) => content-length)
           (else
            ;; NOTE this calculates the directory listing twice:
            ;; once here, and once for the actuall listing.
            (bytevector-length (directory-listing self)))))

    ((symlink) (bytevector-length (string->utf8 (readlink (path self)))))
    (else => (lambda (type) (throw 'http 403 (format #f "Can't access content of ~a file" type))))))

(define-method (content-type (self <file-resource>))
  (case (stat:type (lstat (path self)))
    ((symlink) "text/plain")
    ((regular)
     (cond
      ((get-xattr (path self)
                  (string-append (xattr-prefix) ".mime")
                  follow-symlinks?: #f)
       => utf8->string)
      (else
       ;; TODO actually reference a propper mime database
       (case (string->symbol (last (string-split (path self) #\.)))
         ((txt)  "text/plain; charset=utf-8")
         ((html) "text/html; charset=utf-8")
         ((xml)  "application/xml")
         (else   #f)))))
    ((directory) "text/html; charset=utf-8")
    (else #f)))

(define-method (set-getcontenttype! (self <file-resource>) value)
  (lambda ()
   (set-xattr! (path self) (string-append (xattr-prefix) ".mime")
               (string->utf8 (xml-text-content value))
               follow-symlinks?: #f)))

(define-method (remove-getcontenttype! (self <file-resource>))
  (lambda ()
   (remove-xattr! (path self) (string-append (xattr-prefix) ".mime")
                  follow-symlinks?: #f)))

(define-method (display-name (self <file-resource>))
  (and=>
   (get-xattr (path self)
              (string-append (xattr-prefix) ".displayname")
              follow-symlinks?: #f)
   utf8->string))

(define-method (set-displayname! (self <file-resource>) value)
  (lambda () (set-xattr! (path self)
                    (string-append (xattr-prefix) ".displayname")
                    (string->utf8 (xml-text-content value))
                    follow-symlinks?: #f)))

(define-method (remove-displayname! (self <file-resource>))
  (lambda () (remove-xattr! (path self)
                       (string-append (xattr-prefix) ".displayname")
                       follow-symlinks?: #f)))


(define-method (last-modified (self <file-resource>))
  (-> (path self)
      lstat stat:mtime
      unix-time->datetime))

(define-method (set-dead-property!! (self <file-resource>) value)
  (typecheck value xml-element?)

  (lambda ()
    (set-xattr! (path self)
                (format #f "~a.~a"
                        (xattr-property-prefix)
                        (xml-element-hash-key value))
                (string->utf8
                 (with-output-to-string
                   (lambda () (namespaced-sxml->xml value))))
                follow-symlinks?: #f)))


(define-method (get-dead-property (self <file-resource>) value)
  (cond
   ((get-xattr (path self)
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
              (list-xattr (path self) follow-symlinks?: #f)))


(define-method (remove-dead-property!! (self <file-resource>) value)
  (typecheck value xml-element?)
  (lambda ()
    (remove-xattr!
     (path self)
     (format #f "~a.~a" (xattr-property-prefix) (xml-element-hash-key value))
     follow-symlinks?: #f)))



(define-method (on-child-removed (parent <file-resource>) (child <resource>)))

(define-method (move-resource-implementation!
                (source <file-resource>)
                (destination <file-resource>)
                name)
  (rename-file (path source) (path-append (path destination) name)))

(define-method (remove-self! (resource <file-resource>))
  (delete-file-recursively (path resource))
  (when (parent resource)
    (on-child-removed (parent resource) resource)))

(define-method (create-collection! (resource <file-resource>) name headers body)
  (when body (throw 'http 415))
  (catch 'system-error
    (lambda ()
      (define new-path (path-append (path resource) name))
      (mkdir new-path)
      (make <file-resource>
        parent: resource
        path: new-path))
    (lambda (_ __ fmt args data)
      (cond ((= EEXIST (car data)) (throw 'http 405))
            ((= EACCES (car data)) (throw 'http 403))
            ((= ENOENT (car data)) (throw 'http 409))
            (else (throw 'http 500))))))

(define-method (create-resource! (resource <file-resource>) name)
  (let ((p (path-append (path resource) name)))
    (let ((fp (open p (logior O_RDWR O_CREAT O_EXCL))))
      (close fp))
    (make <file-resource> parent: resource path: p)))

(define-method (mount-resource! (resource <resource>) (parent <file-resource>)
                                name)
  (throw 'http 502 "Resources can't be \"mounted\" under file resources."))

;; (define-method (mount-resource! (resource <file-resource>) (parent <file-resource>)
;;                                 name)
;;   (let-env ((olddir (path other))
;;             (newdir (path-append (path resource) name)))
;;            (system "mount --bind \"$olddir\" \"newdir\"")))



