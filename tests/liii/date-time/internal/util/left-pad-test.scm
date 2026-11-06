;; left-pad
;; 在字符串左侧填充指定字符至目标长度。
;;
;; 语法
;; ----
;; (left-pad str c min-length)
;;
;; 参数
;; ----
;; str : string? 待填充字符串。
;; c : char? 填充字符。
;; min-length : integer? 目标长度。
;;
;; 返回值
;; ----
;; string? 长度不小于 min-length 的新字符串；已达长度时返回原串。

(import (liii check) (liii date-time internal util))

(check-set-mode! 'report-failed)

(check (left-pad "5" #\0 2) => "05")
(check (left-pad "abc" #\0 2) => "abc")
(check (left-pad "" #\- 3) => "---")
(check (left-pad "42" #\0 4) => "0042")

(check-report)
