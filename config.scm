;;; Preliminary config file for the system.
;;; Currently loaded by main, and requires that `calendar-files`
;;; is set to a list of files (or directories).

(use-modules (ice-9 regex)
             (sxml simple)

             ;; TODO this module introduces description-filter. It should be
             ;; possible to use set-config! before the declaration point is
             ;; known. But I currently get a config error.
             ;; (vcomponent datetime output)
             )

(set-config! 'calendar-files (glob "~/.local/var/cal/*"))

;;; TODO possibly replace with propper lookup
(define my-courses
  '((TSEA82 . "Datorteknik")
    (TFYA19 . "Kvantdatorer")
    (TATA42 . "Tvåvarren")
    (TSRT04 . "Matlab")
    (TDDC78 . "Paralellprogrammering")
    (TDDB68 . "Pintos")))

(define* (aref alist key optional: default)
  (or (assoc-ref alist key) default key))

(set-config! 'summary-filter
 (lambda (ev str)
   (regexp-substitute/global
    #f "T[A-Z]{3}[0-9]{2}" str
    'pre (lambda (m) (aref my-courses (string->symbol (match:substring m))))
    'post)))

(define (a link) `(a (@ (href ,link)) ,link))

(define (parse-html str)
  ;; extra space to ensure that we don't get a self closing
  ;; div tag in the final output
  ;; TODO Fix a real sxml->html   | The space
  ;; method instead of pretending |
  ;; that HTML is XML.            v
  (xml->sxml (string-append "<div> " str "</div>")
             default-entity-handler:
             (lambda (port name)
               (case name
                 [(nbsp) " "]
                 [else (symbol->string name)]))) )

(define (parse-links str)
  (define regexp (make-regexp "https?://\\S+"))
  (let recur ((str str))
    (let ((m (regexp-exec regexp str)))
      (if (not m)
          (list str)
          (cons* (match:prefix m)
                 (a (match:substring m))
                 (recur (match:suffix m)))))))

(define html-cals
  '("D-sektionens officiella kalender"
    "LiTHe kod"
    "Klassfadder 2020"))

(set-config! 'description-filter
 (lambda (ev str)
   (cond [(member (prop (parent ev) 'NAME)
                  html-cals
                  )
          (parse-html (regexp-substitute/global
                        #f "<br>" str
                        'pre "<br/>" 'post))]
         [else (parse-links str)])))

(set-config! 'week-start mon)
(set-config! 'default-calendar "Calendar")
(set-config! 'path-prefix (car (glob "~/.local")))
