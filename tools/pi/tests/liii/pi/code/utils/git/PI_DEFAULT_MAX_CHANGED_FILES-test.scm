;; PI_DEFAULT_MAX_CHANGED_FILES
;; 默认收集变更文件的数量上限。
;;
;; 语法
;; ----
;; PI_DEFAULT_MAX_CHANGED_FILES
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; integer?
;; 默认最大文件变更数，数值为 10。

(import (liii check) (liii pi code utils git))

(check-set-mode! 'report-failed)

;; 验证默认最大文件变更数
(check PI_DEFAULT_MAX_CHANGED_FILES => 10)

(check-report)
