(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-tool-execution-end
;; 构造 tool_execution_end 工具执行完毕事件对象。
(let ((ev (pi-event-tool-execution-end "c1" "bash" "res" #f)))
  (check (cdr (assoc "type" ev)) => "tool_execution_end")
  (check (cdr (assoc "isError" ev)) => #f))

(check-report)
