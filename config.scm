;;; Preliminary config file for the system.
;;; Currently loaded by main, and requires that `calendar-files`
;;; is set to a list of files (or directories).


(use-modules (srfi srfi-26)
             (ice-9 ftw))

(define calendar-files
    (let ((path (string-append (getenv "HOME") "/.calendars/")))
      (map (cut string-append path <>)
           (scandir path (lambda (str) (not (char=? #\. (string-ref str 0))))))))
