;; string-find-char
;; 从 start-pos 起查找指定字符的索引位置。
;;
;; 语法
;; ----
;; (string-find-char str char start-pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; char : char
;; 目标字符。
;; start-pos : integer
;; 起始索引。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 找到返回索引，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link))

(check-set-mode! 'report-failed)

(check (string-find-char "abc" #\b 0) => 1)
(check (string-find-char "abc" #\z 0) => #f)

(check-report)
