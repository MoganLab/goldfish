(import (liii check) (liii pi ai event) (liii pi ai message))

(check-set-mode! 'report-failed)

;; pi-event-tool-execution-start
;; 构造 tool_execution_start 工具开始执行事件对象。
(let* ((args '(("command" . "ls")))
       (ev (pi-event-tool-execution-start "c1" "bash" args)))
  (check-true (pi-event-tool-execution-start? ev))
  (check (pi-event-tool-execution-start-type ev) => "tool_execution_start")
  (check (pi-event-tool-execution-start-tool-call-id ev) => "c1")
  (check (pi-event-tool-execution-start-tool-name ev) => "bash")
  (check (pi-event-tool-execution-start-args ev) => args)
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "tool_execution_start")
  (check (cdr (assoc "toolCallId" ev)) => "c1")
  (check (cdr (assoc "toolName" ev)) => "bash")
  (check (cdr (assoc "args" ev)) => args))

(check-report)
