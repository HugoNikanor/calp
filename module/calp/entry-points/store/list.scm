(define-module (calp entry-points store list)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 format)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module (hnh util color)
  :use-module (vcomponent data-stores common)
  :use-module (web uri)
  :export (%summary main))

(define-public %summary
  (G_ "List available stores."))

(define opt-spec '())

(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (define zone
    (or (option-ref opts 'tz #f)
        (getenv "TZ")
        ((@ (datetime localtime) get-localtime))))

  (format #t "== ~a ==~%"
          (G_ "Configured Stores"))
  (for (href . store) in ((@ (vcomponent config) data-stores))
       (display "- ")
       (awhen (store-color store)
              (let ((r g b _ (->rgb/values it)))
                (format #t "\x1b[48;2;~a;~a;~am  \x1b[m "
                        r g b)))

       (format #t "~a~%" (store-displayname store))
       (format #t "  href: ~a~%" href)
       (format #t "  ~@?~%" (G_ "~a entries") (entry-count store))
       (awhen (store-description store)
              ;; TODO flow text
              (format #t "    ~a~%" it))
       ;; TODO? store-calendar-timezone
       (format #t "  ~a~%" (uri->string (store-uri store)))
       )


  (newline)
  )
