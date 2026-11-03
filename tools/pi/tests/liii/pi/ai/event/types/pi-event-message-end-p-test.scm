;; pi-event-message-end?
;; 判断对象是否为消息结束事件 record 实例。
;;
;; 语法
;; ----
;; (pi-event-message-end? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于消息结束事件 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-message-end "final 消息")))
  (check-true (pi-event-message-end? obj))
) ;let
(check-false (pi-event-message-end? '()))
(check-false (pi-event-message-end? #f))
(check-false (pi-event-message-end? "not-a-record"))

(check-report)
