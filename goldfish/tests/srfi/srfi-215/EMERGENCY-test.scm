;; EMERGENCY
;; 系统不可用。
;;
;; 语法
;; ----
;; EMERGENCY
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; exact-integer
;; 日志级别常量，值为 0。
;;
;; 说明
;; ----
;; EMERGENCY 表示系统不可用状态，是 RFC 5424 中最严重的日志级别。
;; 级别取值来自 RFC 5424 Syslog 协议，数值越小越严重（0 最严重，7 最轻微）。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-215))


(check-set-mode! 'report-failed)


;; 日志级别常量
(check EMERGENCY => 0)
(check (exact? EMERGENCY) => #t)
(check (integer? EMERGENCY) => #t)

;; 级别取值落在 RFC 5424 定义的 0..7 区间内（数值越小越严重）
(check (and (<= 0 EMERGENCY) (<= EMERGENCY 7)) => #t)


(check-report)
