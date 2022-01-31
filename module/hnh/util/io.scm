(define-module (hnh util io)
  :use-module ((ice-9 rdelim) :select (read-line)))

(define-public (open-input-port str)
  (if (string=? "-" str)
      (current-input-port)
      (open-input-file str)))

(define-public (open-output-port str)
  (if (string=? "-" str)
      (current-output-port)
      (open-output-file str)))



(define-public (read-lines port)
  (with-input-from-port port
    (lambda ()
      (let loop ((line (read-line)))
        (if (eof-object? line)
            '() (cons line (loop (read-line))))))))

;; Same functionality as the regular @var{with-output-to-file}, but
;; with the difference that either everything is written, or nothing
;; is written, and if anything is written it's all written atomicaly at
;; once (the original file will never contain an intermidiate state).
;; Does NOT handle race conditions between threads.
;; Return #f on failure, something truthy otherwise
(define-public (with-atomic-output-to-file filename thunk)
  ;; copy to enusre writable string
  (define tmpfile (string-copy (string-append
                                (dirname filename)
                                file-name-separator-string
                                "." (basename filename)
                                "XXXXXX")))
  (define port #f)
  (dynamic-wind
    (lambda () (set! port (mkstemp! tmpfile)))
    (lambda ()
      (with-output-to-port port thunk)
      ;; Closing a port forces a write, due to buffering
      ;; some of the errors that logically would come
      ;; from write calls are first raised here. But since
      ;; crashing is acceptable here, that's fine.
      (close-port port)
      (rename-file tmpfile filename))
    (lambda ()
      (when (access? tmpfile F_OK)
        ;; I'm a bit unclear on how to trash our write buffer.
        ;; hopefully first removing the file, followed by closing
        ;; the port is enough for the kernel to do the sensible
        ;; thing.
        (delete-file tmpfile)
        (close-port port)
        ;; `when' defaults to the truthy `()', see (calp util)
        ;; (note that #<unspecified> is thruthy, but shouldn't be
        ;; counted on, since anything with an unspecified return
        ;; value might as well return #f)
        #f))))
