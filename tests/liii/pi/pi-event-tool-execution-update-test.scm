(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-tool-execution-update
;; 构造 tool_execution_update 工具执行过程增量输出事件对象。
(let ((ev (pi-event-tool-execution-update "c1" "bash" '() "part")))
  (check (cdr (assoc "type" ev)) => "tool_execution_update"))

(check-report)
