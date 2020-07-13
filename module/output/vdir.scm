;;; Commentary:
;;; Module for writing components to the vdir storage format.
;;; Currently also has some cases for "big" icalendar files,
;;; but those are currently unsupported.
;;; Code:

(define-module (output vdir)
  :use-module (util)
  :use-module (output ical)
  :use-module (vcomponent)
  )

(define / file-name-separator-string)

(define-public (save-event event)
  (define calendar (parent event))
  (case (prop calendar 'X-HNH-SOURCETYPE)
    [(file)
     (error "Importing into direct calendar files not supported")]

    [(vdir)
     (let* ((uid (or (prop event 'UID) (uuidgen)))
            ;; copy to enusre writable string
            (tmpfile (string-copy (string-append (prop calendar 'X-HNH-DIRECTORY)
                                                 / ".calp-" uid "XXXXXX")))
            (port (mkstemp! tmpfile)))
       (set! (prop event 'UID) uid)
       (with-output-to-port port
         (lambda () (print-components-with-fake-parent (list event))))
       ;; does close flush?
       (force-output port)
       (close-port port)
       (rename-file tmpfile (string-append (prop calendar 'X-HNH-DIRECTORY)
                                           / uid ".ics"))
       uid)]

    [else
     (error "Source of calendar unknown, aborting.")
     ]))
