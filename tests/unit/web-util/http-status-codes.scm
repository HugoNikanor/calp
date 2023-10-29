(define-module (test http-status-codes)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((web http status-codes)
               :select (http-status-codes
                        http-status-phrase
                        http-status-line)))

(test-equal "Existing code" "Locked" (http-status-phrase 423))
(test-equal "Missing code" "" (http-status-phrase 999))
(test-equal "Default status line"
  "HTTP/1.1 302 Found" (http-status-line 302))
(test-equal "Custom status line"
  "HTTP/1.1 200 Sure Thing" (http-status-line 200 "Sure Thing"))

'((web http status-codes))
