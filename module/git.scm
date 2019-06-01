(define-module (git)
  :use-module (util)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module ((ice-9 popen) :select (open-input-pipe))
  :export (get-git-version))

(define (get-git-version)
  (-> "git rev-parse HEAD"
      open-input-pipe
      read-line))


