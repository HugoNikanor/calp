;;; Commentary:
;;; Wrapper around the xattr interface.
;;; Assumes that all paths and names are UTF-8 strings,
;;; but leaves all values as bytevectors.
;;; Code:

(define-module (xattr)
  :use-module (system foreign)
  :use-module ((rnrs bytevectors gnu) :select (bytevector-slice))
  :use-module ((scheme base) :select (bytevector-append
                                      string->utf8
                                      bytevector-length
                                      bytevector-u8-ref
                                      make-bytevector
                                      utf8->string))
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :export (set-xattr!
           get-xattr
           list-xattr
           remove-xattr!))


(define (string->c-string str)
  (bytevector->pointer
   (bytevector-append (string->utf8 str) #vu8(0))))

;; Find first index in bytevector whose value satisfies predicate.
(define* (bytevector-find pred bv optional: (start 0))
  (let loop ((i start))
    (cond ((= i (bytevector-length bv)) #f)
          ((pred (bytevector-u8-ref bv i)) i)
          (else (loop (1+ i))))))

;; Extract all C strings from a bytevector.
;; Assumes that nothing follows the final nul byte
(define (extract-c-strings bv)
  (let loop ((i 0))
    (cond ((bytevector-find (lambda (b) (= b 0)) bv i)
           => (lambda (end)
                (cons (bytevector-slice bv i (- end i))
                      (loop (1+ end)))))
          (else '()))))



(define lib (dynamic-link))

(define setxattr
  (pointer->procedure
   int (dynamic-func "setxattr" lib)
   `(* * * ,size_t ,int)
   return-errno?: #t))

(define getxattr
  (pointer->procedure
   ssize_t (dynamic-func "getxattr" lib)
   `(* * * ,size_t)
   return-errno?: #t))

(define removexattr
  (pointer->procedure
   int (dynamic-func "removexattr" lib)
   `(* *)
   return-errno?: #t))

(define listxattr
  (pointer->procedure
   ssize_t (dynamic-func "listxattr" lib)
   `(* * ,size_t)
   return-errno?: #t))


(define fsetxattr
  (pointer->procedure
   int (dynamic-func "fsetxattr" lib)
   `(* * * ,size_t ,int)
   return-errno?: #t))

(define fgetxattr
  (pointer->procedure
   ssize_t (dynamic-func "fgetxattr" lib)
   `(* * * ,size_t)
   return-errno?: #t))

(define fremovexattr
  (pointer->procedure
   int (dynamic-func "fremovexattr" lib)
   `(* *)
   return-errno?: #t))

(define flistxattr
  (pointer->procedure
   ssize_t (dynamic-func "flistxattr" lib)
   `(* * ,size_t)
   return-errno?: #t))


(define lsetxattr
  (pointer->procedure
   int (dynamic-func "lsetxattr" lib)
   `(* * * ,size_t ,int)
   return-errno?: #t))

(define lgetxattr
  (pointer->procedure
   ssize_t (dynamic-func "lgetxattr" lib)
   `(* * * ,size_t)
   return-errno?: #t))

(define lremovexattr
  (pointer->procedure
   int (dynamic-func "lremovexattr" lib)
   `(* *)
   return-errno?: #t))

(define llistxattr
  (pointer->procedure
   ssize_t (dynamic-func "llistxattr" lib)
   `(* * ,size_t)
   return-errno?: #t))

;; TODO these should be sourced from <sys/xattr.h>
(define XATTR_CREATE  1)
(define XATTR_REPLACE 2)




;; Returns an unspecified value on success, and throws on failure
(define* (set-xattr! path-or-port name value
                     key: force
                     (follow-symlinks? #t))
  (define-values (ret errno)
    ((cond ((port? path-or-port) fsetxattr)
           (follow-symlinks?      setxattr)
           (else                 lsetxattr))
     ((if (port? path-or-port) port->fdes string->c-string)
      path-or-port)
     (string->c-string name)
     (bytevector->pointer value)
     (bytevector-length value)
     (case force
       ((create)  XATTR_CREATE)
       ((replace) XATTR_REPLACE)
       (else 0))))
  (when (= ret -1)
    (scm-error 'system-error "set-xattr!"
               "Failed setting xattr, ~a: ~s" (list (strerror errno) path-or-port)
               (list errno))))

;; returns a bytevector on success, #f if no such value, and throws on error
(define* (get-xattr path-or-port name key: (follow-symlinks? #t))
  (define proc (cond ((port? path-or-port) fgetxattr)
                     (follow-symlinks?      getxattr)
                     (else                 lgetxattr)))
  (define path* ((if (port? path-or-port) port->fdes string->c-string)
      path-or-port))
  (define name* (string->c-string name))
  (define-values (ret errno)
    (proc path* name* %null-pointer 0))
  (when (and (= ret -1)
             (not (= errno ENODATA)))
    (scm-error 'system-error "get-xattr"
               "Failed getting buffer size, ~a: ~s"
               (list (strerror errno) path-or-port)
               (list errno)))
  (if (= errno ENODATA)
      #f
      (let* ((bv (make-bytevector ret))
             (ret errno (proc path* name* (bytevector->pointer bv) ret)))
        (when (and (= ret -1)
                   (not (= errno ENODATA)))
          (scm-error 'system-error "get-xattr"
                     "Failed getting xattr, ~a: ~s"
                     (list (strerror errno) path-or-port)
                     (list errno)))
        (if (= errno ENODATA)
            #f bv))))

;; Returns a list of strings, or throws on error
(define* (list-xattr path-or-port key: (follow-symlinks? #t))
  (define proc (cond ((port? path-or-port) flistxattr)
                     (follow-symlinks?      listxattr)
                     (else                 llistxattr)))
  (define path* ((if (port? path-or-port) port->fdes string->c-string)
                 path-or-port))
  (define-values (ret errno) (proc path* %null-pointer 0))
  (when (= ret -1)
    (scm-error 'system-error "list-xattr"
               "Failed getting buffer size, ~a: ~s"
               (list (strerror errno) path-or-port)
               (list errno)))
  (let* ((bv (make-bytevector ret))
         (ret errno (proc path* (bytevector->pointer bv) ret)))
    (when (= ret -1)
      (scm-error 'system-error "list-xattr"
                 "Failed listing attrs, ~a: ~s"
                 (list (strerror errno) path-or-port)
                 (list errno)))
    (map utf8->string (extract-c-strings bv))))

;; Returns #t if successfully removed the attribute, #f if no such
;; attribute existed, and throws on error otherwise.
(define* (remove-xattr! path-or-port name key: (follow-symlinks? #t))
  (define-values (ret errno)
    ((cond ((port? path-or-port) fremovexattr)
           (follow-symlinks?      removexattr)
           (else                 lremovexattr))
     ((if (port? path-or-port) port->fdes string->c-string)
      path-or-port)
     (string->c-string name)))
  (when (and (= ret -1)
             (not (= errno ENODATA)))
    (scm-error 'system-error "remove-xattr!"
               "Failed removing xattr, ~a: ~s"
               (list (strerror errno) path-or-port)
               (list errno)))
  (not (= errno ENODATA)))
