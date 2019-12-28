;;; Preliminary config file for the system.
;;; Currently loaded by main, and requires that `calendar-files`
;;; is set to a list of files (or directories).

(use-modules (srfi srfi-26)
             (srfi srfi-88)
             (ice-9 regex)
             (ice-9 rdelim)
             (glob))

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
