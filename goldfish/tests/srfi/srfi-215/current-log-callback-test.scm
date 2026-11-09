;; current-log-callback
;; 读取或设置当前日志回调过程。
;;
;; 语法
;; ----
;; (current-log-callback)
;; (current-log-callback callback)
;;
;; 参数
;; ----
;; callback : procedure 或 #f
;; 日志回调过程，接收一条日志条目（关联列表）；
;; 设为 #f 时 send-log 静默丢弃日志。
;;
;; 返回值
;; ----
;; procedure 或 #f
;; 读取时返回当前的回调过程；
;; 设置时返回被设置的回调。
;;
;; 说明
;; ----
;; 默认回调是一个不产生任何输出的空过程。
;; 每次 send-log 发送日志时，都会用当前回调处理日志条目。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-215))


(check-set-mode! 'report-failed)


;; 默认值是过程
(check (procedure? (current-log-callback)) => #t)

;; 设置后可以读回同一个过程
(define cb (lambda (log-entry) (values)))
(current-log-callback cb)
(check (eq? (current-log-callback) cb) => #t)

;; 回调会收到 send-log 发送的日志条目
(define *received* #f)
(current-log-callback (lambda (msg) (set! *received* msg)))
(send-log WARNING "hello")
(check (cdr (assq 'MESSAGE *received*)) => "hello")
(check (cdr (assq 'SEVERITY *received*)) => 4)

;; 设为 #f 时 send-log 静默丢弃
(current-log-callback #f)
(check (current-log-callback) => #f)
(send-log INFO "dropped")

;; 恢复默认回调
(current-log-callback (lambda (log-entry) (values)))
(check (procedure? (current-log-callback)) => #t)


(check-report)
