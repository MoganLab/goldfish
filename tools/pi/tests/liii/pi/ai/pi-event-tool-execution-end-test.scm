(import (liii check) (liii pi ai event) (liii pi ai message))

(check-set-mode! 'report-failed)

;; pi-event-tool-execution-end
;; 构造 tool_execution_end 工具执行完毕事件对象。
(let ((ev (pi-event-tool-execution-end "c1" "bash" "res" #f)))
  (check-true (pi-event-tool-execution-end? ev))
  (check (pi-event-tool-execution-end-type ev) => "tool_execution_end")
  (check (pi-event-tool-execution-end-tool-call-id ev) => "c1")
  (check (pi-event-tool-execution-end-tool-name ev) => "bash")
  (check (pi-event-tool-execution-end-result ev) => "res")
  (check (pi-event-tool-execution-end-is-error ev) => #f)
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "tool_execution_end")
  (check (cdr (assoc "toolCallId" ev)) => "c1")
  (check (cdr (assoc "toolName" ev)) => "bash")
  (check (cdr (assoc "result" ev)) => "res")
  (check (cdr (assoc "isError" ev)) => #f))

(check-report)
