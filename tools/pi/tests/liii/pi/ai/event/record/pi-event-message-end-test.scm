;; pi-event-message-end
;; 构造 message_end 事件对象。
;;
;; 语法
;; ----
;; (pi-event-message-end message)
;;
;; 参数
;; ----
;; message : any?
;; 消息对象。
;;
;; 返回值
;; ----
;; pi-event-message-end?
;; 消息结束事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-message-end '(("role" . "assistant")))))
  (check-true (pi-event-message-end? ev))
  (check (pi-event-message-end-type ev) => "message_end")
  (check (pi-event-message-end-message ev) => '(("role" . "assistant")))
) ;let

(check-report)
