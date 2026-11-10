;; pi-thinking-levels
;; 思考级别全集（由弱到强）。
;;
;; 语法
;; ----
;; pi-thinking-levels
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含各思考级别字符串的列表。

(import (liii check) (liii pi ai thinking))

(check-set-mode! 'report-failed)

(define %levels '("off" "minimal" "low" "medium" "high" "xhigh" "max"))

(check pi-thinking-levels => %levels)

(check-report)
