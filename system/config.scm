(use-modules (calp util config)
             (ice-9 regex)
             ((datetime) :select (mon))
             (glob))

(set-config! 'calendar-files (glob "/var/lib/calp/.local/var/cal/*"))

(define (parse-links str)
  (define regexp (make-regexp "https?://\\S+"))
  (let recur ((str str))
    (let ((m (regexp-exec regexp str)))
      (if (not m)
          (list str)
          (cons* (match:prefix m)
                 (a (match:substring m))
                 (recur (match:suffix m)))))))

(set-config! 'description-filter
 (lambda (ev str) (parse-links str)))

(set-config! 'week-start mon)
;; (set-config! 'default-calendar "Calendar")

(set-config! 'port 8082)
(set-config! 'edit-mode #t)
