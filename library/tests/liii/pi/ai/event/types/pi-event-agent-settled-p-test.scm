;; pi-event-agent-settled?
;; 判断对象是否为 Agent 任务最终结算事件 record 实例。
;;
;; 语法
;; ----
;; (pi-event-agent-settled? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于 Agent 任务最终结算事件 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-agent-settled #f)))
  (check-true (pi-event-agent-settled? obj))
) ;let
(check-false (pi-event-agent-settled? '()))
(check-false (pi-event-agent-settled? #f))
(check-false (pi-event-agent-settled? "not-a-record"))

(check-report)
