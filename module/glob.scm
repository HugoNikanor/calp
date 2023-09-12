(define-module (glob)
  :use-module (system foreign)
  :use-module (rnrs bytevectors)
  :use-module (c cpp)
  :export (glob))


(define (glob-err epath eerrno)
  (scm-error 'misc-error "glob-err"
             "Glob errored on ~s with errno = ~a"
             (list (pointer->string epath) eerrno)
             #f))

;; NOTE there really should be an (c eval) module, to resolve symbols such as
;; @var{<<}.
(define << ash)
(include# "/usr/include/glob.h" define-public)

(define lib (dynamic-link))

(define glob%
  (pointer->procedure int (dynamic-func "glob" lib)
                      `(* ,int * *)))
(define globfree
  (pointer->procedure void (dynamic-func "globfree" lib)
                      '(*)))

(define glob-flags (logior GLOB_MARK GLOB_BRACE GLOB_TILDE_CHECK))

(define (glob str)
  (let ((bv (make-bytevector 100)))
    (let ((globret (glob% (string->pointer str)
                          glob-flags
                          (procedure->pointer int glob-err (list '* int))
                          (bytevector->pointer bv))))
      (unless (zero? globret)
        (scm-error 'misc-error "glob"
                   "Globret errror ~a"
                   (list globret)
                   #f))
      (let* ((globstr (parse-c-struct (bytevector->pointer bv) (list size_t '* size_t)))
             (strvec (pointer->bytevector (cadr globstr) (car globstr) 0
					  (string->symbol (format #f "u~a" (* 8 (sizeof '*))))))
             (ret (map (compose pointer->string make-pointer)
                       (bytevector->uint-list strvec (native-endianness) (sizeof '*)))))

        (globfree (bytevector->pointer bv))
        ret))))
