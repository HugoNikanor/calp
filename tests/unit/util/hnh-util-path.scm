(define-module (test hnh-util-path)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((hnh util env) :select (with-working-directory))
  :use-module (hnh util path))

(test-equal
  "no slashes"
  "home/user"
  (path-append "home" "user"))

(test-equal
  "no slashes, absolute"
  "/home/user"
  (path-append "" "home" "user"))

(test-equal
  "slashes in one component, absolute"
  "/home/user"
  (path-append "" "/home/" "user"))

(test-equal
  "slashes in one component, absolute due to first"
  "/home/user"
  (path-append "/home/" "user"))

(test-equal
  "Slashes in both"
  "home/user"
  (path-append "home/" "/user"))

(test-equal "root" "/" (path-append ""))

(test-equal
  '("usr" "lib" "test")
  (path-split "usr/lib/test"))

(test-equal
  '("usr" "lib" "test")
  (path-split "usr/lib/test/"))

(test-equal
  '("" "usr" "lib" "test")
  (path-split "/usr/lib/test"))

(test-equal
  '("" "usr" "lib" "test")
  (path-split "//usr////lib/test"))

(test-assert (file-hidden? ".just-filename"))
(test-assert (file-hidden? "/path/to/.hidden"))
(test-assert (not (file-hidden? "/visible/.in/hidden")))
(test-assert (not (file-hidden? "")))

;; TODO test realpath with .. and similar

(test-equal "Realpath for path fragment"
  "/home/hugo"
  (with-working-directory
   "/home"
   (lambda () (realpath "hugo"))))

(test-equal "Realpath for already absolute path"
  "/home/hugo"
  (with-working-directory
   "/tmp"
   (lambda () (realpath "/home/hugo"))))

(test-equal "Realpath for already absolute path"
  "/home/hugo"
  (with-working-directory
   "/tmp"
   (lambda () (realpath "/home/hugo"))))


(test-group "Relative to"

  (test-group "With relative child"
    (test-equal "/some/path" (relative-to "/some" "path")))

  ;; Relative parent just adds (getcwd) to start of parent,
  ;; but this is "hard" to test.
  ;; (test-group "With relative parent")

  (test-group "With absolute child"
    (test-error 'misc-error (relative-to "" "/some/path"))
    (test-equal "some/path" (relative-to "/" "/some/path"))
    (test-group "Without trailing slashes"
      (test-equal "path" (relative-to "/some" "/some/path"))
      (test-equal "../path" (relative-to "/some" "/other/path")))
    (test-group "With trailing slashes"
      (test-equal "path" (relative-to "/some" "/some/path/"))
      (test-equal "../path" (relative-to "/some" "/other/path/"))))

  (test-equal "/a/b" (relative-to "/a/b/c" "/a/b"))

  )


(test-equal "Extension of simple file"
  "txt" (filename-extension "file.txt"))

(test-equal "Extension of file with directory"
  "txt" (filename-extension "/direcotry/file.txt"))

(test-equal "Extension of file with multiple"
  "gz" (filename-extension "filename.tar.gz"))

(test-equal "Filename extension when none is present"
  "" (filename-extension "filename"))

(test-equal "Filename extension when none is present, but directory has"
  "" (filename-extension "config.d/filename"))

(test-equal "Filename extension of directory"
  "d" (filename-extension "config.d/"))


(test-equal "Extension of hidden file"
  "sh" (filename-extension ".bashrc.sh"))

(test-equal "Extension of hidden file without extension"
  "bashrc" (filename-extension ".bashrc"))

'((hnh util path))
