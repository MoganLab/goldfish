;; pi-event-tool-execution-end-type
;; 返回 tool-execution-end 事件的固定类型字符串 "tool_execution_end"。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-end-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "tool_execution_end"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-tool-execution-end-type 'dummy) => "tool_execution_end")

(check-report)
