;; pi-event-message-start?
;; 判断对象是否为消息开始事件 record 实例。
;;
;; 语法
;; ----
;; (pi-event-message-start? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于消息开始事件 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-message-start "assistant 消息")))
  (check-true (pi-event-message-start? obj))
) ;let
(check-false (pi-event-message-start? '()))
(check-false (pi-event-message-start? #f))
(check-false (pi-event-message-start? "not-a-record"))

(check-report)
