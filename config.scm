;;; Preliminary config file for the system.
;;; Currently loaded by main, and requires that `calendar-files`
;;; is set to a list of files (or directories).

(use-modules (vcomponent))

(use-modules (srfi srfi-26)
             (srfi srfi-88)
             (ice-9 regex)
             (ice-9 rdelim)
             (sxml simple)
             (glob)
             (datetime util))

(calendar-files (glob "~/.local/var/cal/*"))

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

(summary-filter
 (lambda (ev str)
   (regexp-substitute/global
    #f "T[A-Z]{3}[0-9]{2}" str
    'pre (lambda (m) (aref my-courses (string->symbol (match:substring m))))
    'post)))

(define (a link) `(a (@ (href ,link)) ,link))

(define (parse-html str)
  (xml->sxml (string-append "<div>" str "</div>")
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

(description-filter
 (lambda (ev str)
   (cond [(member (attr (parent ev) 'NAME) '("d_sektionen" "lithekod"))
          (parse-html (regexp-substitute/global
                        #f "<br>" str
                        'pre "<br/>" 'post))]
         [else (parse-links str)])))

(week-start mon)
