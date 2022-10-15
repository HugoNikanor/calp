;;; Commentary:
;;;
;;; For a given module in the project, finds all other modules who uses that
;;; module, and break it down per symbol.
;;;
;;; Code:

(define-module (scripts module-dependants)
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (ice-9 ftw)
  :use-module (ice-9 curried-definitions)
  :use-module (ice-9 format)
  :use-module (texinfo string-utils)
  :use-module (hnh module-introspection)
  :use-module ((hnh module-introspection static-util) :select (get-forms))
  :export (main))

(define %summary "Print all modules which depend on module specified in target file.")
(define %synopsis "module-dependants TARGET-FILE")

(define cstat (make-object-property))

(define (find-all-files-under directory)
  (file-system-fold
   ;; enter?
   (lambda (path stat result) #t)
   ;; leaf
   (lambda (path stat result)
     (set! (cstat path) stat)
     (cons path result))
   ;; down
   (lambda (path stat result)
     (set! (cstat path) stat)
     (cons path result))
   ;; up
   (lambda (path state result) result)
   ;; skip
   (lambda (path stat result) result)
   ;; error
   (lambda (path stat errno result) result)
   '() directory))

(define (regular-file? filename)
  (eq? 'regular (stat:type (cstat filename))))

;; Does @var{filename} have the extension @var{ext}?
(define ((filename-extension? ext) filename)
  (string=? ext (filename-extension filename)))


(define (main . args)
  (define target-file (realpath (car args)))
  (define target-forms
    (reverse (call-with-input-file target-file get-forms)))
  (define target-module
    (find-module-declaration target-forms))
  ;; (define target-symbols (unique-symbols target-forms))
  ;; (write target-module) (newline)

  (define edges
   (concatenate
    (map (lambda (file)
           (catch #t
            (lambda ()
              (define forms (call-with-input-file file get-forms))
              (define module (and=> (-> forms find-module-declaration) resolve-module))
              (define source-symbols (unique-symbols forms))

              (when module
                (awhen (find (lambda (module)
                               (equal? target-module
                                       (module-name module)))
                             (module-uses module))
                       (let ((module-symbols (module-map (lambda (key value) key) it)))
                         ;; (display "    ")
                         (map (lambda (symb)
                                (cons file symb))
                              (lset-intersection eq? source-symbols module-symbols))
                         ))))
            ;; TODO many of these errors are due to the 'prefix and 'postfix
            ;;      read options being set for modules which expect them to be off.
            (lambda (err proc fmt args data)
              (format (current-error-port)
                      "ERROR when reading ~a: ~a in ~a: ~?~%" file err proc fmt args)
              '())))

         (delete target-file
                 (filter (filename-extension? "scm")
                         (filter regular-file?
                                 (append-map (lambda (module-dir)
                                               (find-all-files-under module-dir))
                                             %load-path)))))))


  (define file-uses (make-hash-table))
  (define symbol-used-by (make-hash-table))

  (for-each (lambda (edge)
              (hashq-set! symbol-used-by (cdr edge)
                          (cons (car edge) (hashq-ref symbol-used-by (cdr edge) '())))
              (hash-set! file-uses (car edge)
                         (cons (cdr edge) (hash-ref file-uses (car edge) '()))))
            edges)

  (for-each (lambda (pair)
              (let ((symb files (car+cdr pair)))
                (display (center-string (format #f " ~a (~a uses)" symb (length files))
                                        80 #\= #\=))
                (newline)
                (for-each (lambda (file) (format #t "• ~a~%" file)) files)
                (newline)))
            (sort*
             (hash-map->list cons symbol-used-by)
             string< (compose symbol->string car)))

  (display (center-string " Unused (except possibly internally) " 80 #\= #\=)) (newline)
  (for-each (lambda (symb) (format #t "• ~a~%" symb))
            (lset-difference
             eqv?
             (module-map (lambda (k _) k) (resolve-interface target-module) )
             (hash-map->list (lambda (k _) k) symbol-used-by)))

  )
