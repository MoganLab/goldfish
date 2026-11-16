;; pi-agent-event-type
;; 获取 Agent 事件的类型字符串。
;;
;; 语法
;; ----
;; (pi-agent-event-type ev)
;;
;; 参数
;; ----
;; ev : any
;; Agent 事件对象。
;;
;; 返回值
;; ----
;; string? | #f
;; 返回事件类型字符串；非 Agent 事件返回 #f。

(import (liii check) (liii pi agent event))

(check-set-mode! 'report-failed)

(check (pi-agent-event-type (pi-event-agent-start)) => "agent_start")
(check (pi-agent-event-type (pi-event-turn-start)) => "turn_start")
(check-false (pi-agent-event-type '()))

(check-report)
