;; pi-event-message-update?
;; 判断对象是否为消息增量事件 record 实例。
;;
;; 语法
;; ----
;; (pi-event-message-update? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于消息增量事件 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-message-update '((input . 10) (output . 5)) "delta-1")))
  (check-true (pi-event-message-update? obj))
) ;let
(check-false (pi-event-message-update? '()))
(check-false (pi-event-message-update? #f))
(check-false (pi-event-message-update? "not-a-record"))

(check-report)
