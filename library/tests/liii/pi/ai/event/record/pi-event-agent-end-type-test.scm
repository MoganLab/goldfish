;; pi-event-agent-end-type
;; 返回 agent-end 事件的固定类型字符串 "agent_end"。
;;
;; 语法
;; ----
;; (pi-event-agent-end-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "agent_end"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-agent-end-type 'dummy) => "agent_end")

(check-report)
