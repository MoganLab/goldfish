(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-tool-execution-update
;; 构造 tool_execution_update 工具执行过程增量输出事件对象。
(let* ((args '(("command" . "ls")))
       (part '(("content" . "part1")))
       (ev (pi-event-tool-execution-update "c1" "bash" args part)))
  (check-true (pi-event-tool-execution-update? ev))
  (check (pi-event-tool-execution-update-type ev) => "tool_execution_update")
  (check (pi-event-tool-execution-update-tool-call-id ev) => "c1")
  (check (pi-event-tool-execution-update-tool-name ev) => "bash")
  (check (pi-event-tool-execution-update-args ev) => args)
  (check (pi-event-tool-execution-update-partial-result ev) => part)
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "tool_execution_update")
  (check (cdr (assoc "toolCallId" ev)) => "c1")
  (check (cdr (assoc "toolName" ev)) => "bash")
  (check (cdr (assoc "args" ev)) => args)
  (check (cdr (assoc "partialResult" ev)) => part))

(check-report)
