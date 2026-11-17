;; char-at?
;; 判断字符串 s 在位置 pos 处是否为字符 ch（越界返回 #f）。
;;
;; 语法
;; ----
;; (char-at? s pos len ch)
;;
;; 参数
;; ----
;; s : string
;; 目标字符串。
;; pos : integer
;; 字符位置索引。
;; len : integer
;; 字符串长度。
;; ch : char
;; 期望字符。
;;
;; 返回值
;; ----
;; boolean
;; 未越界且字符相等时返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (char-at? "abc" 1 3 #\b) => #t)
(check (char-at? "abc" 0 3 #\b) => #f)
(check (char-at? "abc" 3 3 #\c) => #f)
(check (char-at? "abc" 9 3 #\a) => #f)
(check (char-at? "" 0 0 #\a) => #f)
(check (char-at? "a\nb" 1 3 #\newline) => #t)

(check-report)
