(import (liii check) (scheme base))

(check-set-mode! 'report-failed)

;; [1312] 基础字符串与字符操作测试

;; 1. string-append
(check (string-append) => "")
(check (string-append "hello") => "hello")
(check (string-append "hello" " " "world") => "hello world")
(check (string-append "scheme" " " "base") => "scheme base")

;; 2. string-length
(check (string-length "") => 0)
(check (string-length "a") => 1)
(check (string-length "hello") => 5)
(check (string-length "goldfish") => 8)

;; 3. string-ref
(check (string-ref "hello" 0) => #\h)
(check (string-ref "hello" 1) => #\e)
(check (string-ref "hello" 4) => #\o)

;; 4. string=?
(check (string=? "" "") => #t)
(check (string=? "abc" "abc") => #t)
(check (string=? "abc" "def") => #f)
(check (string=? "abc" "ABC") => #f)
(check (string=? "foo" "foo" "foo") => #t)
(check (string=? "foo" "foo" "bar") => #f)

;; 5. char=?
(check (char=? #\a #\a) => #t)
(check (char=? #\a #\b) => #f)
(check (char=? #\space #\space) => #t)
(check (char=? #\x #\x #\x) => #t)
(check (char=? #\x #\x #\y) => #f)

(check-report)
