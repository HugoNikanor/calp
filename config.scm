;;; Preliminary config file for the system.
;;; Currently loaded by main, and requires that `calendar-files`
;;; is set to a list of files (or directories).

(use-modules (calp config-base)
             (sxml simple)
             (sxml xpath)
             ;; TODO loading glob is slow (~0.3s). This is really bad
             ;; since we load this file on every program startup (and
             ;; in a way which disables the compiler)
             (glob)
             )

;;; SYSTEMD_EXEC_PID was added in v248 (2021-03-30)
(when (getenv "SYSTEMD_EXEC_PID")
  ((@ (calp entry-points server) port) 58080)
  ((@ (calp html config) debug) #f)
  ((@ (calp html config) edit-mode) #t)
  )

;; ((@ (vcomponent config) calendar-files) (glob "~/.local/var/cal/*"))
((@ (vcomponent config) data-stores)
 (list

  (cons "TDDE18"
        ((@ (vcomponent data-stores file) create-instance)
         path: (car (glob "~/sample-cals/*TDDE18*.ics"))
         media: "text/calendar"))

  (cons "Maskin"
        ((@ (vcomponent data-stores file) create-instance)
         path: (car (glob "~/sample-cals/M1.json"))
         media: "application/calendar+json"))

  (cons "odd-fellow"
        ((@ (vcomponent data-stores file) create-instance)
         path: (car (glob "~/sample-cals/odd-fellow.ics"))
         media: "text/calendar"))

  (cons "VG"
        ((@ (vcomponent data-stores vdir) create-instance)
         path: (car (glob "~/sample-cals/Västgöta Nation"))
         media: "text/calendar"))

  (cons "Calendar"
        ((@ (vcomponent data-stores sqlite) create-instance)
         path: (car (glob "~/sample-cals/cal.db"))))

  ;; ;; NOTE This one is REALLY slow to start
  ;; (cons "Calendar"
  ;;       ((@ (vcomponent data-stores vdir) create-instance)
  ;;        path: "/home/hugo/.local/var/cal/Calendar"
  ;;        media: "text/calendar"))

 ))

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
 (with-source
  (lambda (ev str)
    (regexp-substitute/global
     #f "T[A-Z]{3}[0-9]{2}" str
     'pre (lambda (m) (aref my-courses (string->symbol (match:substring m))))
     'post))))

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

(define html-rx
  (make-regexp "</?\\w+( +\\w+(=[\"']?\\w+[\"']?)?)* */?>"))

((@ (calp html filter) description-filter)
 (with-source
  (lambda (ev str)
    (cond [(prop1 ev 'X-MICROSOFT-SKYPETEAMSMEETINGURL)
           (parse-teams-description str)]
          [(regexp-exec html-rx str)
           (parse-html str)]
          [else (parse-links str)]))))

;;; TODO this is just a parameter currently.
;;; Define it as a configuration value in something
;;; like (calp config) to expose it by `calp get-config`.
((@ (datetime) week-start) mon)

((@ (vcomponent config) default-calendar) "Calendar")
;; (set-config! 'path-prefix (car (glob "~/.local")))
