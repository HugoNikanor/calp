;;; Module for termios interaction from Guile,
;;; Since that for some reason isn't built in.
;; /usr/include/bits/termios.h

(define-module (terminal termios)
  :use-module (system foreign)
  :use-module (ice-9 format)
  :use-module (ice-9 rdelim)
  :use-module (srfi srfi-9)             ; records
  :use-module (util)
  :export (make-termios
           copy-termios
           tcsetattr! tcgetattr! cfmakeraw!))



;;; Constants, TODO, auto parse these from
;; /usr/include/bits/termios.h

(define-public ECHO	0000010)
(define-public ICANON	0000002)

;; @var{when} values
(define-public TCSANOW   0)
(define-public TCSADRAIN 1)
(define-public TCSAFLUSH 2)



;; Create the @code{#$} reader macro for creating C bindings.
;; For example
;; @example
;; #$ tcgetattr : int, * → int
;; @end example
;; would create a binding to a C function called tcgetattr, with the type
;; int, * → int, and bind it to a scheme procedure of the same name.
;; 
;; Currently only links to the library stored in the variable lib, when called.
(eval-when (compile)
 (read-hash-extend
  #\$ (lambda (c port)
        (let* ((name (string-trim-both (read-delimited ":" port)))
               (input (read-delimited "→" port))
               (out (string->symbol (string-trim-both (read-line port)))))
          `(define ,(string->symbol name)
             (pointer->procedure
              ,out (dynamic-func ,name lib)
              (list
               ,@(map (lambda (symb)  (if (eq? symb '*) (quote '*) symb))
                      (map (compose string->symbol string-trim-both)
                           (string-split input #\,))))))))))




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



(define-once lib (dynamic-link))

#$ tcsetattr : int, int, * → int
(define* (tcsetattr! termios  #:optional
                     (port (current-input-port))
                     (when TCSANOW))
  (with-ptr termios (lambda (ptr) (tcsetattr (port->fdes port) when ptr))))


#$ tcgetattr : int, * → int
(define* (tcgetattr! termios #:optional (port (current-input-port)))
  (with-ptr termios (lambda (ptr) (tcgetattr (port->fdes port) ptr))))

#$ cfmakeraw : * → int
(define* (cfmakeraw! termios)
  (with-ptr termios cfmakeraw))
