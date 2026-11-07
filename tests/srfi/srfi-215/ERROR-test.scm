;; ERROR
;; 错误。
;;
;; 语法
;; ----
;; ERROR
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; exact-integer
;; 日志级别常量，值为 3。
;;
;; 说明
;; ----
;; ERROR 表示错误状态。
;; 级别取值来自 RFC 5424 Syslog 协议，数值越小越严重（0 最严重，7 最轻微）。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-215))


(check-set-mode! 'report-failed)


;; 日志级别常量
(check ERROR => 3)
(check (exact? ERROR) => #t)
(check (integer? ERROR) => #t)

;; 级别取值落在 RFC 5424 定义的 0..7 区间内（数值越小越严重）
(check (and (<= 0 ERROR) (<= ERROR 7)) => #t)


(check-report)
