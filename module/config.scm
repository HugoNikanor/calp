;;; Preliminary config file for the system.
;;; Currently loaded by main, and requires that `calendar-files`
;;; is set to a list of files (or directories).


(use-modules (srfi srfi-26)
             (srfi srfi-88)
             (ice-9 ftw)
             (ice-9 regex)
             (ice-9 rdelim))

(define calendar-files
    (let ((path (string-append (getenv "HOME") "/.calendars/")))
      (map (cut string-append path <>)
           (scandir path (lambda (str) (not (char=? #\. (string-ref str 0))))))))

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


(define (summary-filter ev str)
  (regexp-substitute/global
   #f "T[A-Z]{3}[0-9]{2}" str
   'pre (lambda (m) (aref my-courses (string->symbol (match:substring m))))
   'post))
