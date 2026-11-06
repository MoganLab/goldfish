;; DEFAULT-FMT
;; date-time-format/parse 缺省使用的格式化模板。
;;
;; 语法
;; ----
;; DEFAULT-FMT
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; string? 默认格式串。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check DEFAULT-FMT => "%Y-%m-%d %H:%M:%S")

(check-report)
