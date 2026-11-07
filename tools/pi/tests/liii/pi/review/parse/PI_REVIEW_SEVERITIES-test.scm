;; PI_REVIEW_SEVERITIES
;; 代码审查支持的问题严重级别列表。
;;
;; 语法
;; ----
;; PI_REVIEW_SEVERITIES
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 按降序排列的严重级别字符串列表。

(import (liii check) (liii pi review parse))

(check-set-mode! 'report-failed)

;; 验证问题严重级别定义
(check PI_REVIEW_SEVERITIES => '("CRITICAL" "HIGH" "MEDIUM" "INFO"))

(check-report)
