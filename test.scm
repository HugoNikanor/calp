(load "module/main.scm")
(use-modules (vcomponent primitive))

(define root (%vcomponent-make "/home/hugo/Möte grupp.ics"))
(define cal (car (children root)))

(use-modules (ice-9 pretty-print))

(pretty-print (hash-map->list cons (%vcomponent-get-hash-table cal)))
(newline)

(for e in (children cal)
     (pretty-print (hash-map->list cons (%vcomponent-get-hash-table
                                          e)))
     (display (make-string 40 #\-))
     (newline))
