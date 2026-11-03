;; pi-event-message-update-assistant-message-event
;; 获取消息增量事件 record 的 assistant-message-event 字段。
;;
;; 语法
;; ----
;; (pi-event-message-update-assistant-message-event obj)
;;
;; 参数
;; ----
;; obj : any?
;; 消息增量事件 record 实例。
;;
;; 返回值
;; ----
;; any?
;; assistant-message-event 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-message-update '((input . 10) (output . 5)) "delta-1"))
       (obj-b (%make-pi-event-message-update '((input . 20) (output . 8)) "delta-2"))
      ) ;
  (check (pi-event-message-update-assistant-message-event obj-a) => "delta-1")
  (check (pi-event-message-update-assistant-message-event obj-b) => "delta-2")
) ;let*

(check-report)
