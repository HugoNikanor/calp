(define-module (glob)
  :use-module (system foreign)
  :use-module (rnrs bytevectors)
  :export (glob))


(define (glob-err epath eerrno)
  (error "Glob errored on ~s with errno = ~a"
         (pointer->string epath) eerrno))

(eval-when (expand)
  (define has-curly-infix
    (memv 'curly-infix (read-options)))
  (read-enable 'curly-infix))

(define << ash)

(define	GLOB_ERR	{1 << 0})#| Return on read errors.  |#
(define	GLOB_MARK	{1 << 1})#| Append a slash to each name.  |#
(define	GLOB_NOSORT	{1 << 2})#| Don't sort the names.  |#
(define	GLOB_DOOFFS	{1 << 3})#| Insert PGLOB->gl_offs NULLs.  |#
(define	GLOB_NOCHECK	{1 << 4})#| If nothing matches, return the pattern.  |#
(define	GLOB_APPEND	{1 << 5})#| Append to results of a previous call.  |#
(define	GLOB_NOESCAPE	{1 << 6})#| Backslashes don't quote metacharacters.  |#
(define	GLOB_PERIOD	{1 << 7})#| Leading `.' can be matched by metachars.  |#

(define GLOB_MAGCHAR	 {1 <<  8})#| Set in gl_flags if any metachars seen.  |#
(define GLOB_ALTDIRFUNC  {1 <<  9})#| Use gl_opendir et al functions.  |#
(define GLOB_BRACE	 {1 << 10})#| Expand "{a,b}" to "a" "b".  |#
(define GLOB_NOMAGIC	 {1 << 11})#| If no magic chars, return the pattern.  |#
(define GLOB_TILDE	 {1 << 12})#| Expand ~user and ~ to home directories. |#
(define GLOB_ONLYDIR	 {1 << 13})#| Match only directories.  |#
(define GLOB_TILDE_CHECK {1 << 14})#| Like GLOB_TILDE but return an error
				      if the user name is not available.  |#
(eval-when (expand)
  (unless has-curly-infix
    (read-disable 'curly-infix)))

(define-values (glob% globfree%)
  (let ((this (dynamic-link)))
    (values
     (pointer->procedure int  (dynamic-func "glob" this)     `(* ,int * *))
     (pointer->procedure void (dynamic-func "globfree" this) '(*)))))

(define glob-flags (logior GLOB_MARK GLOB_BRACE GLOB_TILDE_CHECK))

(define (glob str)
  (let ((bv (make-bytevector 100)))
    (let*  ((globret (glob% (string->pointer str)
                            glob-flags
                            (procedure->pointer int glob-err (list '* int))
                            (bytevector->pointer bv))))
      (unless (zero? globret)
        (error "Globret errror ~a" globret))
      (let* ((globstr (parse-c-struct (bytevector->pointer bv) (list size_t '* size_t)))
             ;; TODO the 'u64 requires that the system has 64 bit wide pointers...
             (strvec (pointer->bytevector (cadr globstr) (car globstr) 0 'u64))
             (ret (map (compose pointer->string make-pointer)
                       (bytevector->uint-list strvec (native-endianness) (sizeof '*)))))

        (globfree% (bytevector->pointer bv))
        ret))))
