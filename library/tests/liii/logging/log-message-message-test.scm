;; log-message-message
;; 读取日志消息的正文。
;;
;; 语法
;; ----
;; (log-message-message msg)
;;
;; 参数
;; ----
;; msg : list
;; 日志 alist，含 MESSAGE 字段。
;;
;; 返回值
;; ----
;; string
;; 消息正文。
;;
;; 错误处理
;; ----
;; type-error
;; 消息中没有 MESSAGE 字段时抛出。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(check (log-message-message '((SEVERITY . 3) (MESSAGE . "err"))) => "err")

;; 缺少 MESSAGE 字段时报错
(check-catch 'type-error (log-message-message '((SEVERITY . 3))))

(check-report)
