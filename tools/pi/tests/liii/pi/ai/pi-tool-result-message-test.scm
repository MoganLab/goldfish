(import (liii check) (liii pi ai event) (liii pi ai message))

(check-set-mode! 'report-failed)

;; pi-tool-result-message
;; 构造 toolResult 角色消息 record 对象。
(let* ((res-content (list (pi-text-block "file contents here")))
       (msg (pi-tool-result-message "call_123" "read" res-content #f)))
  ;; record 谓词与访问器
  (check-true (pi-tool-result-message? msg))
  (check-true (pi-message? msg))
  (check (pi-message-role msg) => "toolResult")
  (check (pi-tool-result-message-tool-call-id msg) => "call_123")
  (check (pi-tool-result-message-tool-name msg) => "read")
  (check (pi-tool-result-message-content msg) => res-content)
  (check (pi-tool-result-message-is-error msg) => #f)
  (check-true (number? (pi-tool-result-message-timestamp msg)))

  ;; 带错误标记与指定 timestamp
  (let ((err-msg (pi-tool-result-message "call_456" "bash" "failed" #t 99999)))
    (check (pi-tool-result-message-is-error err-msg) => #t)
    (check (pi-tool-result-message-timestamp err-msg) => 99999))

  ;; 兼容 alist 访问
  (check (cdr (assoc "role" msg)) => "toolResult")
  (check (cdr (assoc 'role msg)) => "toolResult")
  (check (cdr (assoc "toolCallId" msg)) => "call_123")
  (check (cdr (assoc 'toolCallId msg)) => "call_123")
  (check (cdr (assoc "toolName" msg)) => "read")
  (check (cdr (assoc 'toolName msg)) => "read")
  (check (cdr (assoc "content" msg)) => res-content)
  (check (cdr (assoc 'content msg)) => res-content)
  (check (cdr (assoc "isError" msg)) => #f)
  (check (cdr (assoc 'isError msg)) => #f))

(check-report)
