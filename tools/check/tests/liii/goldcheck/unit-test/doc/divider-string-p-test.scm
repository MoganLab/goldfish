;; divider-string?
;; 判断字符串是否为标准分割线（去除首尾空白后长度大于 1 且全为连字符 '-'）。
;;
;; 语法
;; ----
;; (divider-string? str)
;;
;; 参数
;; ----
;; str : string  待检测的字符串
;;
;; 返回值
;; ------
;; boolean
;; 符合分割线格式返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldcheck unit-test doc))

(check-set-mode! 'report-failed)

;; 1. 标准分割线
(check (divider-string? "----") => #t)
(check (divider-string? "  ------") => #t)
(check (divider-string? "--") => #t)

;; 2. 非分割线
(check (divider-string? "-") => #f)
(check (divider-string? "") => #f)
(check (divider-string? "---abc---") => #f)
(check (divider-string? "====") => #f)

(check-report)
