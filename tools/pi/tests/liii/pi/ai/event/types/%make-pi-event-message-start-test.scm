;; %make-pi-event-message-start
;; 构造消息开始事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-message-start message)
;;
;; 参数
;; ----
;; message : any?
;; 开始生成的消息对象
;;
;; 返回值
;; ----
;; pi-event-message-start?
;; 消息开始事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-message-start "assistant 消息")))
  (check-true (pi-event-message-start? obj))
  (check (pi-event-message-start-message obj) => "assistant 消息")
) ;let

(check-report)
