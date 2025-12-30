;;; Commentary:
;;; This is (in theory) a configuration file suitable as a system wide
;;; default for when installing calp as a package. It's however
;;; untested, and probably not suitable for use at all.
;;; Code:
(use-modules (calp util config)
             (ice-9 regex)
             ((datetime) :select (mon))
             (glob))

;;; TODO this is replaced with (@ (vcomponent config) data-stores)
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
