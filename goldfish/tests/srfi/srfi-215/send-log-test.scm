;; send-log
;; 发送一条日志消息。
;;
;; 语法
;; ----
;; (send-log severity message field ...)
;;
;; 参数
;; ----
;; severity : exact-integer
;; 日志级别，取值 0 到 7（EMERGENCY 到 DEBUG）。
;;
;; message : string
;; 日志消息正文。
;;
;; field ... : any
;; 附加字段，形如 (key value) 的键值对列表；
;; key 必须是符号，value 为字符串、精确整数或字节串时会原样传递，其它类型会转换为字符串。
;;
;; 返回值
;; ----
;; unspecified
;; 把日志条目交给当前日志回调过程。
;;
;; 说明
;; ----
;; 发送给回调的日志条目是一个关联列表，
;; 首两项固定为 (SEVERITY . severity) 与 (MESSAGE . message)，
;; 其后是本次调用传入的字段与当前日志字段 (current-log-fields)。
;;
;; 错误处理
;; ----
;; type-error 当 severity 不是 0 到 7 的精确整数、message 不是字符串，
;; 或字段列表结构非法（奇数长度或键不是符号）时抛出。

(import (liii check) (srfi srfi-215))


(check-set-mode! 'report-failed)


;; 用于接收回调结果的全局变量
(define *last-msg* #f)

(current-log-callback (lambda (msg) (set! *last-msg* msg)))

;; 基本功能：回调收到 alist 形式的日志条目
(set! *last-msg* #f)
(send-log INFO "test message")
(check (assoc 'SEVERITY *last-msg*) => '(SEVERITY . 6))
(check (assoc 'MESSAGE *last-msg*) => '(MESSAGE . "test message"))

;; 带额外字段
(set! *last-msg* #f)
(send-log WARNING "disk full" 'DISK "/var")
(check (cdr (assq 'SEVERITY *last-msg*)) => 4)
(check (cdr (assq 'MESSAGE *last-msg*)) => "disk full")
(check (cdr (assq 'DISK *last-msg*)) => "/var")

;; 字段值类型：字符串
(set! *last-msg* #f)
(send-log INFO "type test" 'STR "hello")
(check (cdr (assq 'STR *last-msg*)) => "hello")

;; 字段值类型：精确整数
(set! *last-msg* #f)
(send-log INFO "type test" 'NUM 42)
(check (cdr (assq 'NUM *last-msg*)) => 42)

;; 字段值类型：其它类型自动转为字符串
(set! *last-msg* #f)
(send-log INFO "type test" 'SYM 'my-symbol)
(check (string? (cdr (assq 'SYM *last-msg*))) => #t)

;; severity 范围校验
(check-catch 'type-error (send-log -1 "bad"))
(check-catch 'type-error (send-log 8 "bad"))
(check-catch 'type-error (send-log INFO 123))

;; 回调为 #f 时静默丢弃
(current-log-callback #f)
(send-log INFO "should not error")

;; 恢复默认回调
(current-log-callback (lambda (log-entry) (values)))


(check-report)
