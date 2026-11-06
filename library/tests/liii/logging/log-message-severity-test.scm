;; log-message-severity
;; 读取日志消息的严重程度。
;;
;; 语法
;; ----
;; (log-message-severity msg)
;;
;; 参数
;; ----
;; msg : list
;; 日志 alist，含 SEVERITY 字段。
;;
;; 返回值
;; ----
;; exact-integer?
;; 严重程度数值。
;;
;; 错误处理
;; ----
;; type-error
;; 消息中没有 SEVERITY 字段时抛出。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(check (log-message-severity '((SEVERITY . 3) (MESSAGE . "err"))) => 3)
(check (log-message-severity '((SEVERITY . 0) (MESSAGE . "err"))) => 0)

;; 缺少 SEVERITY 字段时报错
(check-catch 'type-error (log-message-severity '((MESSAGE . "err"))))

(check-report)
