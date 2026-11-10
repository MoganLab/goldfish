;; pi-event-agent-settled-type
;; 返回 agent-settled 事件的固定类型字符串 "agent_settled"。
;;
;; 语法
;; ----
;; (pi-event-agent-settled-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "agent_settled"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-agent-settled-type 'dummy) => "agent_settled")

(check-report)
