;; log-debug
;; 以 DEBUG 级别发送一条日志消息。
;;
;; 语法
;; ----
;; (log-debug message)
;; (log-debug message key value ...)
;;
;; 参数
;; ----
;; message : string
;; 日志消息；带额外参数时按 pyfmt 规则格式化。
;; key value ... : any
;; pyfmt 占位符的键值对。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 严重程度低于当前日志级别（current-log-level）时消息被过滤丢弃。
;; 2. 消息按 pyfmt 规则格式化后，连同全局字段交给当前回调。
;;
;; 错误处理
;; ----
;; 消息不是字符串时抛出 type-error。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(define *last-msg* #f)

(log-set-callback! (lambda (msg) (set! *last-msg* msg)))
(log-set-level! DEBUG)

(log-debug "message text")
(check (cdr (assq 'SEVERITY *last-msg*)) => 7)
(check (cdr (assq 'MESSAGE *last-msg*)) => "message text")

;; pyfmt 格式化
(set! *last-msg* #f)
(log-debug "User %(name)s logged in" :name "Alice")
(check (cdr (assq 'MESSAGE *last-msg*)) => "User Alice logged in")

(check-report)
