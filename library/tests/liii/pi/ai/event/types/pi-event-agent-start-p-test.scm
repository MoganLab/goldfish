;; pi-event-agent-start?
;; 判断对象是否为 Agent 循环启动事件 record 实例。
;;
;; 语法
;; ----
;; (pi-event-agent-start? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于 Agent 循环启动事件 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-agent-start)))
  (check-true (pi-event-agent-start? obj))
) ;let
(check-false (pi-event-agent-start? '()))
(check-false (pi-event-agent-start? #f))
(check-false (pi-event-agent-start? "not-a-record"))

(check-report)
