;;; Preliminary config file for the system.
;;; Currently loaded by main, and requires that `calendar-files`
;;; is set to a list of files (or directories).

(use-modules (ice-9 regex)
             (sxml simple)
             (sxml xpath)
             )

((@ (vcomponent config) calendar-files) (glob "~/.local/var/cal/*"))

(define my-courses
  '((TSEA82 . "Datorteknik")
    (TFYA19 . "Kvantdatorer")
    (TATA42 . "Tvåvarren")
    (TSRT04 . "Matlab")
    (TDDC78 . "Paralellprogrammering")
    (TDDB68 . "Pintos")))

(define* (aref alist key optional: default)
  (or (assoc-ref alist key) default key))

((@ (calp html filter) summary-filter)
 (lambda (ev str)
   (regexp-substitute/global
    #f "T[A-Z]{3}[0-9]{2}" str
    'pre (lambda (m) (aref my-courses (string->symbol (match:substring m))))
    'post)))

(define (parse-html str)
  (catch 'misc-error
    ;; resolve-interface throws misc-error on missing module.
    ;; TODO what does html->sxml throw?
    (lambda ()
      (let* ((gumbo (resolve-interface '(sxml gumbo)))
             (html->sxml (module-ref gumbo 'html->sxml)))
        ;; html->sxml always gives us (html (head ...) (body <content>))
        ;; this strips it down to just <content>
        (cdar ((sxpath '(// body)) (html->sxml str)))))
    ;; Give up on parsing
    (lambda _ str)))

(define (a link) `(a (@ (href ,link)) ,link))

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

((@ (calp html filter) description-filter)
 (lambda (ev str)
   (cond [(member (prop (parent ev) 'NAME)
                  html-cals)
          (parse-html str)]
         [(prop ev 'X-MICROSOFT-SKYPETEAMSMEETINGURL)
          (parse-teams-description str)]
         [else (parse-links str)])))

((@ (datetime) week-start) mon)
((@ (vcomponent config) default-calendar) "Calendar")
;; (set-config! 'path-prefix (car (glob "~/.local")))
