(import (liii check)
        (liii goldfmt-rule))

(check-set-mode! 'report-failed)

;; max-inline-length
;; formatter 默认允许 inline 的最大字符串长度。
;;
;; 语法
;; ----
;; max-inline-length
;;
;; 返回值
;; ------
;; integer?
;; 当前默认值为 40。
;;
;; 说明
;; ----
;; `can-inline?` 会先使用 `format-inline` 生成单行候选字符串，
;; 再用该值判断候选字符串是否可以保留在一行。

(check max-inline-length => 40)

(check-report)
