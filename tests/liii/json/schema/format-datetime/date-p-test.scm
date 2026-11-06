;; date?
;; 判断字符串是否为 RFC 3339 完整日期。
;;
;; 语法
;; ----
;; (date? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的完整日期返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 格式固定为 YYYY-MM-DD，年份必须大于 0，日期必须是实际存在的日子，
;; 闰年规则为能被 4 整除且不能被 100 整除，或者能被 400 整除。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-datetime))

(check-set-mode! 'report-failed)

;; 合法日期。

(check-true (date? "2020-01-01"))
(check-true (date? "2020-12-31"))
(check-true (date? "2020-02-29"))
(check-true (date? "2000-02-29"))
(check-true (date? "1999-12-31"))

;; 闰年规则。

(check-false (date? "2021-02-29"))
(check-false (date? "1900-02-29"))
(check-false (date? "2020-02-30"))

;; 月份与日期越界。

(check-false (date? "2020-13-01"))
(check-false (date? "2020-00-10"))
(check-false (date? "2020-01-32"))
(check-false (date? "2020-04-31"))

;; 年份必须大于 0。

(check-false (date? "0000-01-01"))

;; 格式必须是两位补零的 YYYY-MM-DD。

(check-false (date? "2020-1-01"))
(check-false (date? "2020-01-1"))
(check-false (date? "2020/01/01"))
(check-false (date? "2020-01-01T00:00:00Z"))

(check-report)
