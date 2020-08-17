;;; Commentary:
;;; Module for writing components to the vdir storage format.
;;; Currently also has some cases for "big" icalendar files,
;;; but those are currently unsupported.

;;; TODO generalize save-event and remove-event into a general interface,
;;; which different database backends can implement. Actually, do that for all
;;; loading and writing.

;;; Code:

(define-module (output vdir)
  :use-module (util)
  :use-module (vcomponent ical output)
  :use-module (vcomponent)
  :use-module ((util io) :select (with-atomic-output-to-file))
  )


(define / file-name-separator-string)

(define-public (save-event event)
  (define calendar (parent event))
  (case (prop calendar '-X-HNH-SOURCETYPE)
    [(file)
     (error "Importing into direct calendar files not supported")]

    [(vdir)
     (let* ((uid (or (prop event 'UID) (uuidgen))))
       (set! (prop event 'UID) uid
             ;; TODO use existing filename if present?
             (prop event '-X-HNH-FILENAME) (string-append
                                            (prop calendar '-X-HNH-DIRECTORY)
                                            / uid ".ics"))
       (with-atomic-output-to-file (prop event '-X-HNH-FILENAME)
        (lambda () (print-components-with-fake-parent (list event))))
       uid)]

    [else
     (error "Source of calendar unknown, aborting.")
     ]))


(define-public (remove-event event)
  (define calendar (parent event))
  (case (prop calendar '-X-HNH-SOURCETYPE)
    [(file)
     (error "Removing events from large files unsupported")]

    [(vdir)
     (delete-file (prop event '-X-HNH-FILENAME))]

    [else
     (error "Source of calendar unknown, aborting.")
     ]))
