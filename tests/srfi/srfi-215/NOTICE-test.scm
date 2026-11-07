;; NOTICE
;; 通知。
;;
;; 语法
;; ----
;; NOTICE
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; exact-integer
;; 日志级别常量，值为 5。
;;
;; 说明
;; ----
;; NOTICE 表示值得注意但不影响正常功能的状态。
;; 级别取值来自 RFC 5424 Syslog 协议，数值越小越严重（0 最严重，7 最轻微）。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-215))


(check-set-mode! 'report-failed)


;; 日志级别常量
(check NOTICE => 5)
(check (exact? NOTICE) => #t)
(check (integer? NOTICE) => #t)

;; 级别取值落在 RFC 5424 定义的 0..7 区间内（数值越小越严重）
(check (and (<= 0 NOTICE) (<= NOTICE 7)) => #t)


(check-report)
