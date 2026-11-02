;; pi-event-tool-execution-start-type
;; 返回 tool-execution-start 事件的固定类型字符串 "tool_execution_start"。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-start-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "tool_execution_start"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-tool-execution-start-type 'dummy) => "tool_execution_start")

(check-report)
