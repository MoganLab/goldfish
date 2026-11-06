;; send-log
;; 按指定严重程度发送一条日志消息（SRFI-215 兼容入口）。
;;
;; 语法
;; ----
;; (send-log severity message)
;; (send-log severity message field value ...)
;;
;; 参数
;; ----
;; severity : exact-integer?
;; 严重程度，取值 0（EMERGENCY）到 7（DEBUG）。
;; message : string
;; 日志消息正文。
;; field value ... : any
;; 附加字段的键值对，以 plist 形式给出。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 严重程度低于当前日志级别时消息被过滤丢弃。
;; 2. 消息与全局字段合并为 alist 后交给当前回调；
;;    未设置回调时输出到标准错误流。
;;
;; 错误处理
;; ----
;; type-error
;; severity 不是 0~7 的精确整数，或 message 不是字符串时抛出。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(define *last-msg* #f)

(log-set-callback! (lambda (msg) (set! *last-msg* msg)))
(log-set-level! DEBUG)

;; 基础发送
(send-log INFO "srfi-215 compat")
(check (cdr (assq 'SEVERITY *last-msg*)) => 6)
(check (cdr (assq 'MESSAGE *last-msg*)) => "srfi-215 compat")

;; 附加字段
(set! *last-msg* #f)
(send-log INFO "with field" 'APP "test")
(check (cdr (assq 'APP *last-msg*)) => "test")

;; 级别过滤
(log-set-level! WARNING)
(set! *last-msg* #f)
(send-log INFO "should be filtered")
(check *last-msg* => #f)

;; 恢复默认
(log-set-level! WARNING)

;; 参数类型校验
(check-catch 'type-error (send-log -1 "bad"))
(check-catch 'type-error (send-log 8 "bad"))
(check-catch 'type-error (send-log 3.5 "bad"))
(check-catch 'type-error (send-log 'invalid "bad"))
(check-catch 'type-error (send-log INFO 123))
(check-catch 'type-error (send-log INFO 'not-a-string))

(check-report)
