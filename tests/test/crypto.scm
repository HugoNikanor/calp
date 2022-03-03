(use-modules ((crypto) :select (sha256 checksum->string)))

(test-equal "sha256"
  #vu8(24 95 141 179 34 113 254 37 245 97 166 252 147 139 46 38 67 6 236 48 78 218 81 128 7 209 118 72 38 56 25 105)
  (sha256 "Hello"))

(test-equal "sha256 string digest"
  "185f8db32271fe25f561a6fc938b2e264306ec304eda518007d1764826381969"
  (checksum->string (sha256 "Hello")))

(let ((port (open-output-string)))
  (checksum->string (sha256 "Hello") port)
  (test-equal "sha256 string digest to port"
    "185f8db32271fe25f561a6fc938b2e264306ec304eda518007d1764826381969"
    (get-output-string port)))
