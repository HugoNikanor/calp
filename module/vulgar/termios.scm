;;; Module for termios interaction from Guile,
;;; Since that for some reason isn't built in.
;; /usr/include/bits/termios.h

(define-module (vulgar termios)
  :use-module (system foreign)
  :use-module (ice-9 format)
  :use-module (ice-9 rdelim)
  :use-module (srfi srfi-9)             ; records
  :use-module (c cpp)
  :use-module (util)
  :export (make-termios
           copy-termios
           tcsetattr! tcgetattr! cfmakeraw!))



(include# "/usr/include/termios.h" define-public)



(define (empty-values struct-type)
  (cond ((null? struct-type) '())
        ((list? struct-type)
         (cons (empty-values (car struct-type))
               (empty-values (cdr struct-type))))
        (else 0)))



(define struct-termios
  (let ((cc-t uint8)
        (speed-t unsigned-int)
        (tcflag-t unsigned-int)
        (NCCS 32))
   (list tcflag-t tcflag-t tcflag-t tcflag-t
         cc-t (make-list NCCS cc-t)
         speed-t speed-t)))


;; Representation of a termios struct. @var{ptr} should always hold a valid
;; reference to a C object, and @var{list} should hold a parsed version of the
;; same data.
(define-record-type <termios>
  (%make-termios ptr list)
  termios?
  (ptr as-ptr)
  (list as-list))

(define* (make-termios #:optional (data (empty-values struct-termios)))
  (%make-termios (make-c-struct struct-termios data) data))

(define (copy-termios termios)
  (let ((lst (as-list termios)))
   (%make-termios (make-c-struct struct-termios lst) lst)))

;; Sets the pointer value in termios directly. Also parses the data and sets the list.
(define (set-ptr! t v)
  ((record-modifier <termios> 'ptr)  t v)
  ((record-modifier <termios> 'list) t (parse-c-struct v struct-termios)))

;; Sets the list value in termios directly. Also creates a C struct of the data
;; and stores that.
(define (set-list! t v)
  ((record-modifier <termios> 'list) t v)
  ((record-modifier <termios> 'ptr) t (make-c-struct struct-termios v)))

;; TODO {i,o}speed should be looked up in a table.
;; TODO bit fields should display what their fields mean
((@ (srfi srfi-9 gnu) set-record-type-printer!)
 <termios>
 (lambda (t p)
   (format p "#<termios iflag=~b oflag=~b cflag=~b lflag=~b line=~d ispeed=~d ospeed=~d cc=~s>"
           (iflag t) (oflag t) (cflag t) (lflag t) (line t) (ispeed t) (ospeed t)
           (map integer->char (filter (negate zero?) (cc t))))))



;; Macro for creating accessor bindings for slots in a list, which are wrapped
;; inside a <termios> record. Called exactly once below.
(define-macro (create-bindings! . symbols)
  `(begin ,@(map-each
             (lambda (symb i)
               `(define-public ,symb
                  (make-procedure-with-setter
                   (lambda (t) (list-ref (as-list t) ,i))
                   (lambda (t v) (let ((lst (as-list t)))
                              (list-set! lst ,i v)
                              (set-list! t lst))))))
             symbols)))

(create-bindings! ; accessors
 iflag oflag cflag lflag line cc ispeed ospeed)



;; TODO this should possibly use unwind guards
(define (with-ptr termios proc)
  (let ((ptr (as-ptr termios)))
    (let ((ret (proc ptr)))
      (set-ptr! termios ptr)
      ret)))



(define-syntax pointer-quote
  (syntax-rules (*)
    [(_ *) (quote *)]
    [(_ a) a]))

(define-syntax define-foreign
  (syntax-rules (-> →)
    [(_ (name intype ...) -> outtype body ...)
     (define name
       (pointer->procedure
        outtype (begin body ...)
        (list (pointer-quote intype) ...)))]
    [(_ (name intype ...) → outtype body ...)
     (define-foreign (name intype ...) -> outtype body ...)]
    [(_ (name intype ...) body ...)
     (define-foreign (name intype ...) -> void body ...)]))



(define-once lib (dynamic-link))

(define-foreign (tcsetattr int int *) → int
  (dynamic-func "tcsetattr" lib))

(define* (tcsetattr! termios  #:optional
                     (port (current-input-port))
                     (when TCSANOW))
  (with-ptr termios (lambda (ptr) (tcsetattr (port->fdes port) when ptr))))


(define-foreign (tcgetattr int *) → int
  (dynamic-func "tcgetattr" lib))

(define* (tcgetattr! termios #:optional (port (current-input-port)))
  (with-ptr termios (lambda (ptr) (tcgetattr (port->fdes port) ptr))))


(define-foreign (cfmakeraw *)
  (dynamic-func "cfmakeraw" lib))

(define* (cfmakeraw! termios)
  (with-ptr termios cfmakeraw))
