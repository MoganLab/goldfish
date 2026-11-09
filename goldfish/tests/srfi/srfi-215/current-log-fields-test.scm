;; current-log-fields
;; 读取或设置全局默认日志字段。
;;
;; 语法
;; ----
;; (current-log-fields)
;; (current-log-fields fields)
;;
;; 参数
;; ----
;; fields : list
;; 全局默认字段，形如 (key value ...) 的键值对列表；
;; key 必须是符号，值为 #f 的字段会被忽略。
;;
;; 返回值
;; ----
;; list
;; 读取时返回当前的全局默认字段列表；
;; 设置时返回被设置的字段列表。
;;
;; 说明
;; ----
;; 全局默认字段会附加到每一条 send-log 发送的日志条目上。
;;
;; 错误处理
;; ----
;; error 当字段列表长度非法或键不是符号时抛出。

(import (liii check) (srfi srfi-215))


(check-set-mode! 'report-failed)


;; 默认值为空列表
(check (current-log-fields) => '())

;; 设置后可以读回
(current-log-fields '(FACILITY 1))
(check (current-log-fields) => '(FACILITY 1))

;; 全局字段会附加到日志条目中
(define *last-msg* #f)
(current-log-callback (lambda (msg) (set! *last-msg* msg)))
(send-log INFO "with fields")
(check (cdr (assq 'FACILITY *last-msg*)) => 1)

;; 恢复为空
(current-log-fields '())
(check (current-log-fields) => '())

;; 值为 #f 的字段在生成日志条目时被忽略
(current-log-fields '(SKIP #f KEEP 2))
(check (current-log-fields) => '(SKIP #f KEEP 2))
(set! *last-msg* #f)
(send-log INFO "with fields")
(check (cdr (assq 'KEEP *last-msg*)) => 2)
(check (assq 'SKIP *last-msg*) => #f)
(current-log-fields '())


(check-report)
