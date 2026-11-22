;; find-matching-backticks
;; 从 start 起查找长度为 marker-len 的匹配反引号序列起始位置。
;;
;; 语法
;; ----
;; (find-matching-backticks str start marker-len)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; start : integer
;; 起始位置。
;; marker-len : integer
;; 反引号长度。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 找到返回起始索引，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline util))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (find-matching-backticks "`code`" 1 1) => 5)
(check (find-matching-backticks "`unclosed" 1 1) => #f)

(check-report)
