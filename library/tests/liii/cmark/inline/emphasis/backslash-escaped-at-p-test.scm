;; backslash-escaped-at?
;; 判断位置 pos 处的字符是否被前置奇数个反斜杠转义。
;;
;; 语法
;; ----
;; (backslash-escaped-at? str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 位置索引。
;;
;; 返回值
;; ----
;; boolean
;; 被转义返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emphasis))

(check-set-mode! 'report-failed)

(check-true (backslash-escaped-at? "\\*" 1))
(check-false (backslash-escaped-at? "\\\\*" 2))

(check-report)
