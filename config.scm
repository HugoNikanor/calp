;;; Preliminary config file for the system.
;;; Currently loaded by main, and requires that `calendar-files`
;;; is set to a list of files (or directories).

(use-modules (ice-9 regex)
             (sxml simple)
             )

(set-config! 'calendar-files (glob "~/.local/var/cal/*"))

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
  (catch 'misc-error
    (lambda ()
      ;; resolve interface throws on missing module
      (let* ((gumbo (resolve-interface '(sxml gumbo)))
             (html->sxml (module-ref gumbo 'html->sxml)))
        (html->sxml str)))
    ;; Give up on parsing
    (lambda _ str)))

(define (parse-links str)
  (define regexp (make-regexp "https?://\\S+"))
  (let recur ((str str))
    (let ((m (regexp-exec regexp str)))
      (if (not m)
          (list str)
          (cons* (match:prefix m)
                 (a (match:substring m))
                 (recur (match:suffix m)))))))

(define (parse-teams-description str)
  (map (lambda (line)
         (let loop ((line line))
           (cond [(string-match "^_+$" line)
                  '((hr) (br))]
                 ;; hyperlinks go from start of line,
                 ;; or from last pipe character
                 [(string-match "([^|<]*)<([^>]*)>" line)
                  => (lambda (m)
                       (cons*
                        (match:prefix m)
                        `(a (@ (href ,(match:substring m 2)))
                            ,(match:substring m 1))
                        (loop (match:suffix m))))]
                 ;; square brackets are images
                 [(string-match "\\[([^]]+)\\]" line)
                  => (lambda (m)
                       (cons*
                        (match:prefix m)
                        `(img (@ (src ,(match:substring m 1))))
                        (loop (match:suffix m))))]
                 ;; Either the full line, or the remainder
                 ;; after hyperlink and img match.
                 [else (list line '(br))])))
       (string-split str #\newline)))

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
         [(prop ev 'X-MICROSOFT-SKYPETEAMSMEETINGURL)
          (parse-teams-description str)]
         [else (parse-links str)])))

(set-config! 'week-start mon)
(set-config! 'default-calendar "Calendar")
(set-config! 'path-prefix (car (glob "~/.local")))
