;; PI_REVIEW_MAX_FILE_CHARS
;; 单个审查文件截断的最大字符数上限。
;;
;; 语法
;; ----
;; PI_REVIEW_MAX_FILE_CHARS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; integer?
;; 最大字符数上限，数值为 4000。

(import (liii check) (liii pi review files))

(check-set-mode! 'report-failed)

;; 验证单文件最大字符数上限
(check PI_REVIEW_MAX_FILE_CHARS => 4000)

(check-report)
