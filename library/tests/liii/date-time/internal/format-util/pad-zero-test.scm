;; pad-zero
;; 将数字转为字符串并用 '0' 左侧补齐到指定宽度。
;;
;; 语法
;; ----
;; (pad-zero x n)
;;
;; 参数
;; ----
;; x : number? 待转换数字。
;; n : integer? 目标宽度。
;;
;; 返回值
;; ----
;; string? 至少 n 位的数字字符串。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (pad-zero 5 2) => "05")
(check (pad-zero 7 3) => "007")
(check (pad-zero 123 2) => "23")

(check-report)
