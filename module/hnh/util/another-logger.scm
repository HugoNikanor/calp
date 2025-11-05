;;; Commentary:
;;; Draft of a simple structured logger.
;;; In use by parts of this program, but feel free to rip it out if need be.
;;; Code:
(define-module (hnh util another-logger)
  :use-module (hnh util)
  :use-module (datetime)
  :use-module ((web response) :select (response-headers))
  :use-module ((web uri) :select (uri->string))
  :export (log-table
           make-log-table
           log-table-add!

           emit-log!

           log-table-get               ; TODO should -get be exported?
           log-table-format))

;;; Log tables are tables for easily adding key value data,
;;; and later formatting them.
;;; They in themself do not actually do any logging.

;;; The "global" log table
(define log-table (make-parameter #f))

;;; Initialize the global log table to an empty log table
(define (make-log-table) (log-table '()))

;;; Takes a list of alternating symbols and values,
;;; Each such pair is added to the log global table
(define (log-table-add! . args)
  (for (key value) in (group args 2)
       (log-table (acons key value (log-table)))))

;;; Get the given key from the global key table
;;; or return dflt (default #f) if not found
(define* (log-table-get key optional: dflt)
  (or (assoc-ref (log-table) key)
      dflt))

;;; Write data from the global log table to current output port.
;;; Each argument should be one of the following types
;;; string? :: printed verbatim
;;; symbol? :: value looked up in the global log table,
;;;            and value printed
;;; pair? :: The car is a symbol to look up per `symbol?'
;;;          The cdr is a procedure for foramtting the given
;;;          value for output
;;; All other types are ignored.
(define (log-table-format . args)
  (for-each (lambda (arg)
              (cond ((string? arg) (display arg))
                    ((symbol? arg) (cond ((log-table-get arg)
                                          => display)))
                    ((pair? arg)   (cond ((log-table-get (car arg))
                                          => (compose display (cdr arg)))))
                    (else #f)))
            args))

;;; Writes a log message to current error port.
;;; This reads values for the log table.
;;;
;;; The following table fields are used
;;; now :: current datetime, as a datetime?
;;; method :: Name of the source method
;;; uri :: URI accessed, an an uri? object
;;; request :: The source request
;;;             If the request-method of the request is
;;;             'COPY or 'MOVE then `headers' is checked for a
;;;             destination header.
;;; headers :: Request headers, see `request'
;;; response-code :: Response code to emit (e.x. 200)
;;; response-phrase :: Phrase belonging to that code (e.x. "OK")
;;; msg :: Optional freetext message
(define* (emit-log! optional: (port (current-error-port)))
  ;; (write (log-table) (current-error-port))
  ;; (newline (current-error-port))
  (display
   (with-output-to-string
     (lambda ()
       ;; TODO this format should be supplied by the user (of this logging library).
       ;; Currently it's hard coded to a the use case from (calp server webdav)
       (log-table-format
        "< " 'method " " (cons 'uri uri->string) "\n"
        "< " 'response-code " " 'response-phrase "\n"
        "< Completed " (cons 'now (lambda (n) (datetime->string n "~H:~M:~S"))) "\n"
        "< Headers:\n"
        (cons 'response (lambda (r)
                          (string-concatenate
                           (for (name . value) in (response-headers r)
                                (format #f "<     ~a: ~s~%" name value)))))
        )
       ;; (log-table-format (cons 'now (lambda (n) (datetime->string n "~H:~M:~S")))
       ;;                   " " 'method " "
       ;;                   (cons 'uri uri->string)
       ;;                   " ")
       ;; (case (request-method (log-table-get 'request))
       ;;   ((COPY MOVE) (log-table-format
       ;;                 (cons 'headers (lambda (h) (and=> (assoc-ref h 'destination) uri->string)))
       ;;                 " "))
       ;;   (else ""))
       ;; Nginx uses
       ;; <ip> - - [<date>] "<request-line>" <request-status> <content-length> "<referer-url>" "<user-agent>"
       ;; (log-table-format 'response-code " "
       ;;                   'response-phrase
       ;;                   " "
       ;;                   (cons 'headers (lambda (h) (assoc-ref h 'x-litmus)))
       ;;                   "\n")

       (cond ((log-table-get 'msg)
              => (lambda (it)
                   (for line in (string-split (string-trim-both it) #\newline)
                        (format #t "<< ~a~%" line)))))

       (cond ((log-table-get 'backtrace)
              => (lambda (it)
                   (for line in (string-split (string-trim-both it) #\newline)
                        (format #t "<<< ~a~%" line)))))

       (newline)))

   port))
