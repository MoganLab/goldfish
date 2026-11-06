;; duration?
;; 判断字符串是否为 ISO 8601 时长。
;;
;; 语法
;; ----
;; (duration? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的时长返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 格式为 P[nY][nM][nD][T[nH][nM][nS]] 或 P[nW]，
;; 必须以大写 P 开头。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-datetime))

(check-set-mode! 'report-failed)

;; 合法时长。

(check-true (duration? "P1D"))
(check-true (duration? "PT1H"))
(check-true (duration? "P1Y2M3DT4H5M6S"))
(check-true (duration? "P1W"))
(check-true (duration? "P1M"))
(check-true (duration? "PT1M"))
(check-true (duration? "PT0S"))

;; 缺少 P 前缀。

(check-false (duration? "1D"))
(check-false (duration? "2020-01-01"))

;; 只有 P 或 PT 缺少具体数值。

(check-false (duration? "P"))
(check-false (duration? "PT"))

;; T 之后缺少时间部分。

(check-false (duration? "P1DT"))

(check-report)
