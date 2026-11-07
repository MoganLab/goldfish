;; PI_DEFAULT_MAX_BYTES
;; 工具输出截断的默认字节配额
;;
;; 语法
;; ----
;; PI_DEFAULT_MAX_BYTES
;;
;; 参数
;; ----
;; 无（常量）
;;
;; 返回值
;; ------
;; integer?，字节配额

(import (liii check) (liii pi code core tools))

(check-set-mode! 'report-failed)

;; PI_DEFAULT_MAX_BYTES
;; 默认最大输出字节限制 (50KB = 51200)。
(check PI_DEFAULT_MAX_BYTES => 51200)

(check-report)
