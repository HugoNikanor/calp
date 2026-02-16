(define-module (vcomponent media-type)
  :use-module (srfi srfi-88)
  :use-module (srfi srfi-197)
  :use-module (hnh util object)
  :use-module (hnh util type)
  :use-module (ice-9 regex)
  :export (calendar-data-format
           calendar-data-format?
           serializer
           parser
           media-type
           file-extension

           resolve-media-type
           ))


(define-type (calendar-data-format)
  ;; ((parser <format>) <port>)
  parser
  ;; ((serializer <format>) vcomponent <port>)
  serializer
  ;; "text/calendar"
  (media-type type: (or string? false?))
  ;; "ics"
  (file-extension type: (or string? false?))
  )


;;; Given a media-type string, return the corresponding format object
;;; TODO document properly
(define (resolve-media-type media-type-string)
  (define media-type
    (car ((@ (web http) parse-header)
          'content-type media-type-string)))

  (define parts
    (chain (symbol->string media-type)
           (regexp-substitute/global #f "[.]" _ 'pre "-" 'post)
           (string-split _ #\/)
           (map string->symbol _)))
  (module-ref (resolve-interface `(vcomponent media-type ,@parts))
              'format))
