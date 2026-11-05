;; pi-event-agent-end?
;; 判断对象是否为 Agent 循环结束事件 record 实例。
;;
;; 语法
;; ----
;; (pi-event-agent-end? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于 Agent 循环结束事件 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-agent-end '("m1" "m2") #f '((total . 0.01)) #f)))
  (check-true (pi-event-agent-end? obj))
) ;let
(check-false (pi-event-agent-end? '()))
(check-false (pi-event-agent-end? #f))
(check-false (pi-event-agent-end? "not-a-record"))

(check-report)
