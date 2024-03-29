;;; Module for termios interaction from Guile,
;;; Since that for some reason isn't built in.

(define-module (vulgar termios)
  :use-module (system foreign)
  :use-module (ice-9 format)
  :use-module (ice-9 rdelim)
  :use-module (srfi srfi-9)             ; records
  :use-module (srfi srfi-88)
  :use-module (c cpp)
  :use-module (hnh util)
  :export (make-termios
           copy-termios
           tcsetattr! tcgetattr! cfmakeraw!

           iflag oflag cflag lflag line cc ispeed ospeed
           ))



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

(define* (make-termios optional: (data (empty-values struct-termios)))
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

(define (resolve-baud-speed n)
  (cond
   ((= n B0) 0)
   ((= n B50) 50)
   ((= n B75) 75)
   ((= n B110) 110)
   ((= n B134) 134)
   ((= n B150) 150)
   ((= n B200) 200)
   ((= n B300) 300)
   ((= n B600) 600)
   ((= n B1200) 1200)
   ((= n B1800) 1800)
   ((= n B2400) 2400)
   ((= n B4800) 4800)
   ((= n B9600) 9600)
   ((= n B19200) 19200)
   ((= n B38400) 38400)
   ((= n B57600) 57600)
   ((= n B115200) 115200)
   ((= n B230400) 230400)))


;; TODO bit fields should display what their fields mean
((@ (srfi srfi-9 gnu) set-record-type-printer!)
 <termios>
 (lambda (t p)
   (format p "#<termios iflag=~b oflag=~b cflag=~b lflag=~b line=~d ispeed=~d ospeed=~d cc=~s>"
           (iflag t) (oflag t) (cflag t) (lflag t) (line t)
           (resolve-baud-speed (ispeed t))
           (resolve-baud-speed (ospeed t))
           (map integer->char (filter (negate zero?) (cc t))))))



(define (make-termios-accessor idx)
  (make-procedure-with-setter
   (lambda (t) (list-ref (as-list t) idx))
   (lambda (t v) (let ((lst (as-list t)))
              (list-set! lst idx v)
              (set-list! t lst)))))

(define iflag  (make-termios-accessor 0))
(define oflag  (make-termios-accessor 1))
(define cflag  (make-termios-accessor 2))
(define lflag  (make-termios-accessor 3))
(define line   (make-termios-accessor 4))
(define cc     (make-termios-accessor 5))
(define ispeed (make-termios-accessor 6))
(define ospeed (make-termios-accessor 7))



;; Calls procedure with current termios pointer, and restore
;; current pointer afterwards. Allows stuff like turning off echo
;; mode without breaking the calling terminal.
(define (with-termios termios proc)
  (let ((og-ptr #f))
   (dynamic-wind
     (lambda () (set! og-ptr (as-ptr termios)))
     (lambda () (proc (as-ptr termios)))
     (lambda () (set-ptr! termios og-ptr)))))



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

(define* (tcsetattr! termios optional:
                     (port (current-input-port))
                     (when TCSANOW))
  (with-termios termios (lambda (ptr) (tcsetattr (port->fdes port) when ptr))))


(define-foreign (tcgetattr int *) → int
  (dynamic-func "tcgetattr" lib))

(define* (tcgetattr! termios optional: (port (current-input-port)))
  (with-termios termios (lambda (ptr) (tcgetattr (port->fdes port) ptr))))


(define-foreign (cfmakeraw *)
  (dynamic-func "cfmakeraw" lib))

(define* (cfmakeraw! termios)
  (with-termios termios cfmakeraw))
