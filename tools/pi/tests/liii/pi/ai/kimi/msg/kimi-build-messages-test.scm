(import (liii check) (liii pi ai event) (liii pi ai kimi) (liii pi ai message))

(check-set-mode! 'report-failed)

;; kimi-build-messages
;; 将内部消息列表与可选 system-instruction 转换为 OpenAI Chat Completions messages 向量。
(let* ((messages (list (pi-user-message "echo hello")
                       (pi-assistant-message
                         (list (pi-tool-call-block "call_1" "bash" '(("command" . "echo hello")))))
                       (pi-tool-result-message "call_1" "bash" (list (pi-text-block "hello\n")) #f)))
       (vec (kimi-build-messages messages "You are helpful")))
  ;; system + user + assistant + tool = 4 条
  (check (vector-length vec) => 4)
  (let ((s-msg (vector-ref vec 0))
        (u-msg (vector-ref vec 1))
        (a-msg (vector-ref vec 2))
        (t-msg (vector-ref vec 3)))
    (check (cdr (assoc "role" s-msg)) => "system")
    (check (cdr (assoc "content" s-msg)) => "You are helpful")
    (check (cdr (assoc "role" u-msg)) => "user")
    (check (cdr (assoc "content" u-msg)) => "echo hello")
    (check (cdr (assoc "role" a-msg)) => "assistant")
    (let ((tc-vec (cdr (assoc "tool_calls" a-msg))))
      (check (vector-length tc-vec) => 1)
      (let* ((tc (vector-ref tc-vec 0))
             (fn (cdr (assoc "function" tc))))
        (check (cdr (assoc "id" tc)) => "call_1")
        (check (cdr (assoc "type" tc)) => "function")
        (check (cdr (assoc "name" fn)) => "bash")
        (check (string? (cdr (assoc "arguments" fn))) => #t)))
    (check (cdr (assoc "role" t-msg)) => "tool")
    (check (cdr (assoc "tool_call_id" t-msg)) => "call_1")
    (check (cdr (assoc "content" t-msg)) => "hello\n")))

;; 无 system prompt 时
(let* ((messages (list (pi-user-message "hi")))
       (vec (kimi-build-messages messages #f)))
  (check (vector-length vec) => 1)
  (check (cdr (assoc "role" (vector-ref vec 0))) => "user"))

(check-report)
