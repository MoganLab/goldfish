;; pi-event-message-start
;; 构造 message_start 事件对象。
;;
;; 语法
;; ----
;; (pi-event-message-start message)
;;
;; 参数
;; ----
;; message : any?
;; 消息对象。
;;
;; 返回值
;; ----
;; pi-event-message-start?
;; 消息开始事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-message-start '(("role" . "user")))))
  (check-true (pi-event-message-start? ev))
  (check (pi-event-message-start-type ev) => "message_start")
  (check (pi-event-message-start-message ev) => '(("role" . "user")))
) ;let

(check-report)
