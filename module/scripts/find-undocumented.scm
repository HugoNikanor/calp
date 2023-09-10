(define-module (scripts find-undocumented)
  :use-module (srfi srfi-1)
  :use-module (hnh module-introspection all-modules)
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (ice-9 format)
  :use-module (ice-9 regex)
  :use-module (ice-9 rdelim)
  :use-module (rnrs records syntactic)
  :export (main)
  )

(define %summary "Find all uncodumented exported declaration in a project.")

(define %synopsis "find-undocumented <srcdir> <docdir>")

;; (define %help "")

;;; All texinfo forms we want to capture.
;;; For each of these, the following grammar holds:
;;; - The first element should be a string of the texinfo tag to match
;;; - The following arguments are
;;;   - Any number of `_`, meaning an argument we don't care about
;;;   - a single instance of the symbol `name`, which indicates where the name of
;;;     the definition is stored.
;;;   - An optional final argument `...`, which indicates that more may argumnets
;;;     may follow.
(define texinfo-definition-forms
 '(("deffn" _ name ...)
   ("deftp" _ name ...)
   ("defun" name ...)
   ("defmac" name ...)
   ("defspec" name ...)
   ("deftypefn" _ _ name ...)
   ("deftypefun" _ name ...)
   ("defvr" _ name)
   ("defvar" name)
   ("defopt" name)
   ("deftypevr" _ _ name)
   ("deftypevar" _ name)
   ("deftp" _ name ...)
   ("defcv" _ _ name)
   ("deftypecv" _ _ name)
   ("defivar" _ name)
   ("deftypeivar" _ _ name)
   ("defop" _ _ name ...)
   ("deftypeop" _ _ _ name ...)
   ("defmethod" _ name ...)
   ("deftypemethod" _ _ name ...)))

(define (command cmd) (format #f "@ *(~a)x?" cmd))
(define parameter "(\\{(@\\}|[^}])+\\}|[^ \t]+)")
(define rest ".*")
(define regexpes
  (for form in texinfo-definition-forms
       (list
        form
        (string-concatenate
         (intersperse
          "[ \t]*"
          (for (idx symbol) in (enumerate form)
               (cond ((string? symbol)  (command symbol))
                     ((eq? '_ symbol)   parameter)
                     ((eq? '... symbol) rest)
                     ((symbol? symbol)  parameter)
                     (else (scm-error 'misc-error "" "" '() #f)))))))))

(define rxs
  (for (name rx) in regexpes
       (list name
             (make-regexp
              (format #f "^ *~a" rx)
              regexp/newline))))


(define-record-type doc-definition
  (fields symbol type file line))

(define (cmp a b)
  (eq?
   (doc-definition-symbol a)
   (doc-definition-symbol b)))

(define (print-header msg)
  (define middle (format #f "= ~a =" msg))
  (define side (make-string (string-length middle) #\=))
  (format #t "~a~%~a~%~a~%" side middle side))

(define (print-doc-definition def)
  (display (symbol->string (doc-definition-symbol def)))
  (cond ((doc-definition-file def)
         => (lambda (it)
              (display "\t(")
              (display it)
              (cond ((doc-definition-line def)
                     => (lambda (it)
                          (display " ")
                          (display it))))
              (display ")"))))
  (newline))

(define (main . args)
  (define source-directory "module")
  (define doc-dir "doc/ref")
  (define skip-files
    '("module/graphvis.scm"
      "module/glob.scm"))

  (define documented-symbols
    (concatenate
     (for file in (all-files-under-directory doc-dir ".texi")
          (let ((content (call-with-input-file file read-string)))
            (concatenate
             (for (form rx) in rxs
                  (for m in (list-matches rx content)
                       (make-doc-definition
                        (-> m
                            (match:substring
                             ;; Weird offsets to account for how matching groups work
                             (* 2 (1+ (list-index (lambda (x) (eqv? x 'name))
                                                  (cdr form)))))
                            (string-trim-both (string->char-set "{}"))
                            string->symbol)
                        (string->symbol (match:substring m 1))
                        file
                        (1+ (string-count (match:prefix m) (char-set #\newline)))
                        ))))))))

  (define defined-symbols
    (concatenate
     (for path in (all-modules-under-directory source-directory)
          (when (member path skip-files)
            (continue))
          (define components* (drop (path-split path) (length (path-split source-directory))))
          (define name
            (map string->symbol
                 (append (drop-right components* 1)
                         (list (basename (last components*) ".scm")))))
          (catch 'misc-error
            (lambda ()
              (cond ((resolve-interface name)
                     => (lambda (module) (map (lambda (symb) (make-doc-definition symb #f path #f))
                                         (module-map (lambda (k v) k) module))))
                    (else
                     (format (current-error-port) "~s is not a module~%" name)
                     '())))
            (lambda (err proc fmt args data)
              (format (current-error-port) "Failed loading ~s: (~a) ~?~%" name proc fmt args)
              '())))))


  (print-header "Documented functions without (or with private) definitions:")
  (for-each print-doc-definition (lset-difference cmp documented-symbols defined-symbols))
  (newline)

  (print-header "Defined symbols without documentation:")
  (for-each print-doc-definition (lset-difference cmp defined-symbols documented-symbols))

  (newline)
  )
