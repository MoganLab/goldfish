;; Complete base64 string that reproduces the issue from base64.scm line 26
;; This should result in OK (balanced) but our linter incorrectly flags it

(define-constant BYTE2BASE64_BV
  (string->utf8 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))