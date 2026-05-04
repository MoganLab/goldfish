(import (liii check) (srfi srfi-215))

;; srfi-215: Central Log Exchange
;;
;; 测试 SRFI-215 标准接口的完整实现

;; 用于接收回调结果的全局变量
(define *last-msg* #f)

;; 日志级别常量测试
(check EMERGENCY => 0)
(check ALERT => 1)
(check CRITICAL => 2)
(check ERROR => 3)
(check WARNING => 4)
(check NOTICE => 5)
(check INFO => 6)
(check DEBUG => 7)

;; current-log-fields 默认值为空 plist
(check (current-log-fields) => '())

;; current-log-callback 默认值是过程
(check (procedure? (current-log-callback)) => #t)

;; send-log 基本功能：回调接收 alist 形式的日志条目
(set! *last-msg* #f)
(current-log-callback (lambda (msg) (set! *last-msg* msg)))
(send-log INFO "test message")
(check (assoc 'SEVERITY *last-msg*) => '(SEVERITY . 6))
(check (assoc 'MESSAGE *last-msg*) => '(MESSAGE . "test message"))

;; send-log 带额外字段
(set! *last-msg* #f)
(current-log-callback (lambda (msg) (set! *last-msg* msg)))
(send-log WARNING "disk full" 'DISK "/var")
(check (cdr (assq 'SEVERITY *last-msg*)) => 4)
(check (cdr (assq 'MESSAGE *last-msg*)) => "disk full")
(check (cdr (assq 'DISK *last-msg*)) => "/var")

;; current-log-fields 设置全局字段
(set! *last-msg* #f)
(current-log-fields '(FACILITY 1))
(current-log-callback (lambda (msg) (set! *last-msg* msg)))
(send-log INFO "with fields")
(check (cdr (assq 'FACILITY *last-msg*)) => 1)
(current-log-fields '())

;; send-log 字段值类型：字符串
(set! *last-msg* #f)
(current-log-callback (lambda (msg) (set! *last-msg* msg)))
(send-log INFO "type test" 'STR "hello")
(check (cdr (assq 'STR *last-msg*)) => "hello")

;; send-log 字段值类型：精确整数
(set! *last-msg* #f)
(current-log-callback (lambda (msg) (set! *last-msg* msg)))
(send-log INFO "type test" 'NUM 42)
(check (cdr (assq 'NUM *last-msg*)) => 42)

;; send-log 字段值类型：其他类型自动转为字符串
(set! *last-msg* #f)
(current-log-callback (lambda (msg) (set! *last-msg* msg)))
(send-log INFO "type test" 'SYM 'my-symbol)
(check (string? (cdr (assq 'SYM *last-msg*))) => #t)

;; send-log 验证 severity 范围
(check-catch 'wrong-type-arg (send-log -1 "bad"))
(check-catch 'wrong-type-arg (send-log 8 "bad"))
(check-catch 'wrong-type-arg (send-log INFO 123))

;; current-log-callback 为 #f 时静默丢弃
(current-log-callback #f)
(send-log INFO "should not error")

;; 恢复默认回调
(current-log-callback (lambda (log-entry) (values)))

(check-report)
