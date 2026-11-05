;; pi-event-agent-start-type
;; 返回 agent-start 事件的固定类型字符串 "agent_start"。
;;
;; 语法
;; ----
;; (pi-event-agent-start-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "agent_start"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-agent-start-type 'dummy) => "agent_start")

(check-report)
