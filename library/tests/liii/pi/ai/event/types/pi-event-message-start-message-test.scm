;; pi-event-message-start-message
;; 获取消息开始事件 record 的 message 字段。
;;
;; 语法
;; ----
;; (pi-event-message-start-message obj)
;;
;; 参数
;; ----
;; obj : any?
;; 消息开始事件 record 实例。
;;
;; 返回值
;; ----
;; any?
;; message 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-message-start "assistant 消息"))
       (obj-b (%make-pi-event-message-start "user 消息"))
      ) ;
  (check (pi-event-message-start-message obj-a) => "assistant 消息")
  (check (pi-event-message-start-message obj-b) => "user 消息")
) ;let*

(check-report)
