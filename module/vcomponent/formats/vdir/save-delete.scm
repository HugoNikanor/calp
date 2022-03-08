;;; Commentary:
;;; Module for writing components to the vdir storage format.
;;; Currently also has some cases for "big" icalendar files,
;;; but those are currently unsupported.

;;; TODO generalize save-event and remove-event into a general interface,
;;; which different database backends can implement. Actually, do that for all
;;; loading and writing.

;;; Code:

(define-module (vcomponent formats vdir save-delete)
  :use-module (hnh util)
  :use-module (hnh util uuid)
  :use-module ((hnh util path) :select (path-append))
  :use-module ((hnh util exceptions) :select (assert))
  :use-module (vcomponent formats ical output)
  :use-module (vcomponent)
  :use-module ((hnh util io) :select (with-atomic-output-to-file))
  )


(define-public (save-event event)
  (define calendar (parent event))

  (assert (eq? 'vdir (prop calendar '-X-HNH-SOURCETYPE)))

  (let* ((uid (or (prop event 'UID) (uuid))))
    (set! (prop event 'UID) uid
      ;; TODO use existing filename if present?
      (prop event '-X-HNH-FILENAME) (path-append
                                      (prop calendar '-X-HNH-DIRECTORY)
                                      (string-append uid ".ics")))
    (with-atomic-output-to-file (prop event '-X-HNH-FILENAME)
      (lambda () (print-components-with-fake-parent (list event))))
    uid))


(define-public (remove-event event)
  (define calendar (parent event))
  (assert (eq? 'vdir (prop calendar '-X-HNH-SOURCETYPE)))
  (delete-file (prop event '-X-HNH-FILENAME)))
