;;; Commentary:
;;; Module for writing components to the vdir storage format.
;;; Currently also has some cases for "big" icalendar files,
;;; but those are currently unsupported.
;;; Code:

(define-module (output vdir)
  :use-module (util)
  :use-module (output ical)
  :use-module (vcomponent)
  :use-module ((util io) :select (with-atomic-output-to-file))
  )


(define / file-name-separator-string)

(define-public (save-event event)
  (define calendar (parent event))
  (case (prop calendar 'X-HNH-SOURCETYPE)
    [(file)
     (error "Importing into direct calendar files not supported")]

    [(vdir)
     (let* ((uid (or (prop event 'UID) (uuidgen))))
       (set! (prop event 'UID) uid)
       (with-atomic-output-to-file
        (string-append (prop calendar 'X-HNH-DIRECTORY) / uid ".ics")
        (lambda () (print-components-with-fake-parent (list event))))
       uid)]

    [else
     (error "Source of calendar unknown, aborting.")
     ]))
