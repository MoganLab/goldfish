(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-tool-execution-start
;; 构造 tool_execution_start 工具开始执行事件对象。
(let ((ev (pi-event-tool-execution-start "c1" "bash" '(("command" . "ls")))))
  (check (cdr (assoc "type" ev)) => "tool_execution_start")
  (check (cdr (assoc "toolName" ev)) => "bash"))

(check-report)
