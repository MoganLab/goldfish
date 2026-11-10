;; pi-event-message-end-message
;; 获取消息结束事件 record 的 message 字段。
;;
;; 语法
;; ----
;; (pi-event-message-end-message obj)
;;
;; 参数
;; ----
;; obj : any?
;; 消息结束事件 record 实例。
;;
;; 返回值
;; ----
;; any?
;; message 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-message-end "final 消息"))
       (obj-b (%make-pi-event-message-end "other 消息"))
      ) ;
  (check (pi-event-message-end-message obj-a) => "final 消息")
  (check (pi-event-message-end-message obj-b) => "other 消息")
) ;let*

(check-report)
