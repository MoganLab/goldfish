;; log-set-level!
;; 设置日志级别阈值。
;;
;; 语法
;; ----
;; (log-set-level! severity)
;;
;; 参数
;; ----
;; severity : exact-integer?
;; 级别阈值，取值 0（EMERGENCY）到 7（DEBUG）。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 严重程度数值大于等于阈值的消息才会被发送；
;; 缺省阈值为 WARNING（4）。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(define *last-msg* #f)

(log-set-callback! (lambda (msg) (set! *last-msg* msg)))

;; 级别过滤：阈值越大越宽松（severity <= 阈值才发送）
(log-set-level! DEBUG)
(set! *last-msg* #f)
(log-debug "should pass filter")
(check (cdr (assq 'MESSAGE *last-msg*)) => "should pass filter")

(log-set-level! INFO)
(set! *last-msg* #f)
(log-debug "should be filtered")
(check *last-msg* => #f)

(log-info "should pass filter")
(check (cdr (assq 'MESSAGE *last-msg*)) => "should pass filter")

;; 恢复默认
(log-set-level! WARNING)

(check-report)
