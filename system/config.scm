(use-modules (calp util config)
             (ice-9 regex)
             ((datetime) :select (mon))
             (glob))

((@ (vcomponent) calendar-files) (glob "/var/lib/calp/.local/var/cal/*"))

(define (parse-links str)
  (define regexp (make-regexp "https?://\\S+"))
  (let recur ((str str))
    (let ((m (regexp-exec regexp str)))
      (if (not m)
          (list str)
          (cons* (match:prefix m)
                 (a (match:substring m))
                 (recur (match:suffix m)))))))

((@ (calp html vcomponent) description-filter)
 (lambda (ev str) (parse-links str)))

((@ (datetime) week-start) mon)
;; (set-config! 'default-calendar "Calendar")

((@ (calp entry-points server) port) 8082)
((@ (calp html config) edit-mode) #t)
