;; %make-pi-event-message-end
;; 构造消息结束事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-message-end message)
;;
;; 参数
;; ----
;; message : any?
;; 结束时的消息对象
;;
;; 返回值
;; ----
;; pi-event-message-end?
;; 消息结束事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-message-end "final 消息")))
  (check-true (pi-event-message-end? obj))
  (check (pi-event-message-end-message obj) => "final 消息")
) ;let

(check-report)
