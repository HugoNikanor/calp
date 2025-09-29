(define-module (calp webdav resource file utils)
  :use-module (srfi srfi-1)
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (datetime)
  :use-module (ice-9 popen)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module ((ice-9 ftw) :select (scandir))
  :export (file-creation-date
           mimetype
           directory?
           delete-file-recursively
           ))


(define (file-creation-date path)
  (-> path lstat stat:ctime unix-time->datetime))

(define (mimetype path)
  (let ((pipe (open-pipe* OPEN_READ "file" "--brief" "--mime-type"
                          path)))
    (begin1 (read-line pipe)
            (close-pipe pipe))))

(define (directory? path)
   (eq? 'directory (stat:type (lstat path))))

;;; $ rm -r "$path"
(define (delete-file-recursively path)
  (cond ((directory? path)
         (for-each (lambda (child)
                     (delete-file-recursively (path-append path child)))
                   (remove (lambda (p) (member p '("." "..")))
                           (scandir path)))
         (rmdir path))
        (else (delete-file path))))
