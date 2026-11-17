;; ordered-list-start-number
;; 提取有序列表项的起始编号。
;;
;; 语法
;; ----
;; (ordered-list-start-number line)
;;
;; 参数
;; ----
;; line : string
;; 有序列表项行。
;;
;; 返回值
;; ----
;; integer
;; 返回编号整数。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (ordered-list-start-number "42. a") => 42)

(check-report)
