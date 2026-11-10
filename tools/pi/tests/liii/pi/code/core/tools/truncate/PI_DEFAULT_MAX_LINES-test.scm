;; PI_DEFAULT_MAX_LINES
;; 工具输出截断的默认行数配额
;;
;; 语法
;; ----
;; PI_DEFAULT_MAX_LINES
;;
;; 参数
;; ----
;; 无（常量）
;;
;; 返回值
;; ------
;; integer?，行数配额

(import (liii check) (liii pi code core tools))

(check-set-mode! 'report-failed)

;; PI_DEFAULT_MAX_LINES
;; 默认最大输出行数限制 (2000)。
(check PI_DEFAULT_MAX_LINES => 2000)

(check-report)
