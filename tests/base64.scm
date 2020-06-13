(((util base64) base64encode base64decode))

;; Examples from RFC4648

(test-equal "" (base64encode ""))
(test-equal "Zg==" (base64encode "f"))
(test-equal "Zm8=" (base64encode "fo"))
(test-equal "Zm9v" (base64encode "foo"))
(test-equal "Zm9vYg==" (base64encode "foob"))
(test-equal "Zm9vYmE=" (base64encode "fooba"))
(test-equal "Zm9vYmFy" (base64encode "foobar"))

(test-equal "" (base64decode ""))
(test-equal "f" (base64decode "Zg=="))
(test-equal "fo" (base64decode "Zm8="))
(test-equal "foo" (base64decode "Zm9v"))
(test-equal "foob" (base64decode "Zm9vYg=="))
(test-equal "fooba" (base64decode "Zm9vYmE="))
(test-equal "foobar" (base64decode "Zm9vYmFy"))
