;; max-inline-length
;; formatter 默认允许 inline 的最大字符串长度。
;;
;; 语法
;; ----
;; max-inline-length
;;
;; 参数
;; ----
;; 无
;; 该值为常量，不接受任何参数。
;;
;; 返回值
;; ------
;; integer?
;; 当前默认值为 80。
;;
;; 说明
;; ----
;; `can-inline?` 会先使用 `format-inline` 生成单行候选字符串，
;; 再用该值判断候选字符串是否可以保留在一行。

(import (liii check) (liii goldfmt rule))

(check-set-mode! 'report-failed)


(check max-inline-length => 80)

(check-report)
