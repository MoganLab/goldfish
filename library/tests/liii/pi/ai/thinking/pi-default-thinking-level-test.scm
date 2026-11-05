;; pi-default-thinking-level
;; 用户未显式指定时的默认级别。
;;
;; 语法
;; ----
;; pi-default-thinking-level
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string?
;; 默认思考级别字符串（"medium"）。

(import (liii check) (liii pi ai thinking))

(check-set-mode! 'report-failed)

(check pi-default-thinking-level => "medium")

(check-report)
