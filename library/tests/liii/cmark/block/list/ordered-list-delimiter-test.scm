;; ordered-list-delimiter
;; 提取有序列表项的分隔符字符。
;;
;; 语法
;; ----
;; (ordered-list-delimiter line)
;;
;; 参数
;; ----
;; line : string
;; 有序列表项行。
;;
;; 返回值
;; ----
;; char 或 #f
;; 返回 'period 或 'one-paren 或 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (ordered-list-delimiter "1. a") => 'period)
(check (ordered-list-delimiter "2) b") => 'one-paren)

(check-report)
