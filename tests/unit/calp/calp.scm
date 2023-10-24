(define-module (test calp)
  :use-module (srfi srfi-64)
  :use-module (ice-9 regex)
  :use-module (calp))

(test-assert "Calp version is a proper version string"
             (string-match "^[0-9]+[.][0-9]+([.][0-9]+)?(-.*)?$"
                          version))

(test-assert "Prodid returns a string"
             (string? (prodid)))

'((calp))
