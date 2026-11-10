;; CRITICAL
;; 日志级别常量：严重故障（严重程度 2）。
;;
;; 语法
;; ----
;; CRITICAL
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; exact-integer?
;; 严重程度数值 2。
;;
;; 说明
;; ----
;; 与 SRFI-215 的级别定义一致，数值越小越严重；
;; send-log 以该数值作为 severity 参数，
;; log-set-level! 以该数值作为过滤阈值。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(check CRITICAL => 2)

(check-report)
