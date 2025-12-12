(define-module (test language)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util env)
  :use-module (hnh util language))

(test-equal 'sv
  (let-env ((LC_MESSAGES "sv_SE.UTF-8")
            (LC_ALL "en_DK.UTF-8"))
           (resolve-language)))

(test-equal 'fi
  (let-env ((LC_MESSAGES "")
            (LC_ALL "fi_DK.UTF-8"))
           (resolve-language)))

(test-equal 'en
  (let-env ((LC_MESSAGES "")
            (LC_ALL ""))
           (resolve-language)))

'((hnh util language))
