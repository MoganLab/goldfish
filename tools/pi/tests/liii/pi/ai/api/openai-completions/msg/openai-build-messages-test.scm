(import (liii check) (liii pi ai event) (liii pi ai api openai-completions) (liii pi ai message))

(check-set-mode! 'report-failed)

;; openai-build-messages
;; 将内部消息历史转换为 OpenAI Chat Completions messages。
;;
;; 语法
;; ----
;; (openai-build-messages messages system-instruction)
;;
;; 参数
;; ----
;; messages : list
;; 内部 user、assistant 和 toolResult 消息列表。
;; system-instruction : string | #f
;; 可选系统提示词；非空白字符串会生成首条 system message。
;;
;; 返回值
;; ----
;; vector
;; OpenAI messages 向量，元素为对应角色的 alist。
;;
;; 说明
;; ----
;; assistant 的多个文本块按顺序拼接，工具参数序列化为 JSON 字符串。
;; toolResult 映射为 tool 角色并保留 tool_call_id。
;; 空 content 的 assistant 消息（如 ESC abort 后 stopReason=aborted 的残留）
;; 被整体跳过，不产生空 assistant API 消息（对齐上游 blocks 为空时 continue），
;; 跳过后允许产生连续 user 消息。
;;
;; 示例
;; ----
;; (openai-build-messages (list (pi-user-message "hi")) #f) => #((("role" . "user") ...))
(let* ((messages (list (pi-user-message "echo hello")
                       (pi-assistant-message
                         (list (pi-tool-call-block "call_1" "bash" '(("command" . "echo hello")))))
                       (pi-tool-result-message "call_1" "bash" (list (pi-text-block "hello\n")) #f)))
       (vec (openai-build-messages messages "You are helpful")))
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
       (vec (openai-build-messages messages #f)))
  (check (vector-length vec) => 1)
  (check (cdr (assoc "role" (vector-ref vec 0))) => "user"))

;; assistant 的多个文本块按顺序拼接
(let* ((messages (list (pi-assistant-message
                         (list (pi-text-block "hello ")
                               (pi-text-block "world")))))
       (vec (openai-build-messages messages #f))
       (msg (vector-ref vec 0)))
  (check (cdr (assoc "content" msg)) => "hello world"))

;; ESC abort 后空 content 的 assistant 消息不得进入 API 请求：
;; 跳过后允许产生连续 user 消息（对齐上游 blocks 为空时 continue 的行为）
(let* ((messages (list (pi-user-message "hello")
                       (pi-assistant-message '() :stop-reason "aborted")
                       (pi-user-message "world")))
       (vec (openai-build-messages messages #f)))
  (check (vector-length vec) => 2)
  (check (cdr (assoc "role" (vector-ref vec 0))) => "user")
  (check (cdr (assoc "role" (vector-ref vec 1))) => "user"))

;; 非空 assistant 消息正常保留
(let* ((messages (list (pi-assistant-message (list (pi-text-block "ok")))))
       (vec (openai-build-messages messages #f)))
  (check (vector-length vec) => 1)
  (check (cdr (assoc "role" (vector-ref vec 0))) => "assistant")
  (check (cdr (assoc "content" (vector-ref vec 0))) => "ok"))

(check-report)
