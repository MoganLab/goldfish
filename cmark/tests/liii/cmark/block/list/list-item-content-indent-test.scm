;; list-item-content-indent
;; 计算列表项内容的起始缩进列数。
;;
;; 语法
;; ----
;; (list-item-content-indent line)
;;
;; 参数
;; ----
;; line : string
;; 列表项标记行。
;;
;; 返回值
;; ----
;; integer
;; 返回内容缩进列数。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (list-item-content-indent "- a") => 2)

(check-report)
