;; pi-event-tool-execution-update-type
;; 返回 tool-execution-update 事件的固定类型字符串 "tool_execution_update"。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-update-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "tool_execution_update"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-tool-execution-update-type 'dummy) => "tool_execution_update")

(check-report)
