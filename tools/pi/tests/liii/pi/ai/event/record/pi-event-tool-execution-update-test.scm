;; pi-event-tool-execution-update
;; 构造 tool_execution_update 事件对象。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-update tool-call-id tool-name args partial-result)
;;
;; 参数
;; ----
;; tool-call-id : string?
;; 工具调用标识符。
;;
;; tool-name : string?
;; 工具名称。
;;
;; args : any?
;; 工具参数。
;;
;; partial-result : any?
;; 部分输出增量结果。
;;
;; 返回值
;; ----
;; pi-event-tool-execution-update?
;; 工具执行更新事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-tool-execution-update "c1" "bash" '() "part1")))
  (check-true (pi-event-tool-execution-update? ev))
  (check (pi-event-tool-execution-update-type ev) => "tool_execution_update")
  (check (pi-event-tool-execution-update-partial-result ev) => "part1")
) ;let

(check-report)
