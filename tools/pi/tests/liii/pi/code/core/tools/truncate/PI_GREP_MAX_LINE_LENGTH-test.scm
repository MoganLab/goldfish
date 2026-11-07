;; PI_GREP_MAX_LINE_LENGTH
;; grep 单行输出的默认最大长度
;;
;; 语法
;; ----
;; PI_GREP_MAX_LINE_LENGTH
;;
;; 参数
;; ----
;; 无（常量）
;;
;; 返回值
;; ------
;; integer?，单行长度上限

(import (liii check) (liii pi code core tools))

(check-set-mode! 'report-failed)

;; PI_GREP_MAX_LINE_LENGTH
;; grep 单行输出最大字符截断长度 (500)。
(check PI_GREP_MAX_LINE_LENGTH => 500)

(check-report)
