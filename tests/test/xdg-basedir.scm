(define-module (test xdg-basedir)
  :use-module (srfi srfi-64)
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module (srfi srfi-88)
  :use-module ((hnh util env) :select (let-env))
  )


(let-env ((HOME "/home/user")
          (XDG_DATA_HOME #f)
          (XDG_CONFIG_HOME #f)
          (XDG_STATE_HOME #f)
          (XDG_DATA_DIRS #f)
          (XDG_CONFIG_DIRS #f)
          (XDG_CACHE_HOME #f)
          (XDG_RUNTIME_DIR #f))
 (test-group "Defaults"
   (test-equal "XDG_DATA_HOME" "/home/user/.local/share"
     (xdg-data-home))
   (test-equal "XDG_CONFIG_HOME" "/home/user/.config"
     (xdg-config-home))
   (test-equal "XDG_STATE_HOME" "/home/user/.local/state"
     (xdg-state-home))
   (test-equal "XDG_DATA_DIRS" (xdg-data-dirs)
     '("/usr/local/share" "/usr/share"))
   (test-equal "XDG_CONFIG_DIRS" '("/etc/xdg")
     (xdg-config-dirs))
   (test-equal "XDG_CACHE_HOME" "/home/user/.cache"
     (xdg-cache-home))
   (let ((warning
          (with-error-to-string
            (lambda ()
              (test-equal "XDG_RUNTIME_DIR"
                "/tmp" (xdg-runtime-dir))))))
     (test-assert "The warning actually contains something"
       (< 0 (string-length warning)))))

 (test-group "Custom values"
   (let-env ((XDG_DATA_HOME "/a"))
            (test-equal "XDG_DATA_HOME" "/a" (xdg-data-home)))
   (let-env ((XDG_CONFIG_HOME "/b"))
            (test-equal "XDG_CONFIG_HOME" "/b" (xdg-config-home)))
   (let-env ((XDG_STATE_HOME "/c"))
            (test-equal "XDG_STATE_HOME" "/c" (xdg-state-home)))
   (let-env ((XDG_DATA_DIRS "/d:/e"))
            (test-equal "XDG_DATA_DIRS" '("/d" "/e") (xdg-data-dirs)))
   (let-env ((XDG_CONFIG_DIRS "/f:/g"))
            (test-equal "XDG_CONFIG_DIRS" '("/f" "/g") (xdg-config-dirs)))
   (let-env ((XDG_CACHE_HOME "/h"))
            (test-equal "XDG_CACHE_HOME" "/h" (xdg-cache-home)))
   (let ((warning
          (with-error-to-string
            (lambda ()
              (let-env ((XDG_RUNTIME_DIR "/i"))
                       (test-equal "XDG_RUNTIME_DIR" "/i" (xdg-runtime-dir)))))))
     (test-assert "No error was emitted"
       (string-null? warning)))))

