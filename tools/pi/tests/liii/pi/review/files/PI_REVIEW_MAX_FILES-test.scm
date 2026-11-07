;; PI_REVIEW_MAX_FILES
;; 单次代码审查允许处理的最大文件数量。
;;
;; 语法
;; ----
;; PI_REVIEW_MAX_FILES
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; integer?
;; 最大文件数量限制，数值为 10。

(import (liii check)
        (liii pi review files)
) ;import

(check-set-mode! 'report-failed)

;; 验证最大文件数常量
(check PI_REVIEW_MAX_FILES => 10)

(check-report)
