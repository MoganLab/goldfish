;; anthropic-parse-response
;; 解析 Anthropic API 返回的 JSON 响应，转换为内部 assistant 消息。
;;
;; 语法
;; ----
;; (anthropic-parse-response json-data status-code)
;;
;; 参数
;; ----
;; json-data : alist
;; 已解析的 Anthropic 响应 JSON 对象。
;; status-code : integer
;; HTTP 状态码，用于错误提示。
;;
;; 返回值
;; ----
;; pi-assistant-message
;; 内部 assistant 消息，包含 content、stopReason、usage 与 errorMessage。

(import (liii check)
  (liii pi ai api anthropic-messages core)
  (liii pi ai event)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; 普通文本响应
(let* ((json-resp '(("id" . "msg_123")
                    ("type" . "message")
                    ("role" . "assistant")
                    ("content" . #((("type" . "text") ("text" . "Done!"))))
                    ("model" . "claude-3-7-sonnet-20250219")
                    ("stop_reason" . "end_turn")
                    ("usage" . (("input_tokens" . 10) ("output_tokens" . 5)))))
       (parsed (anthropic-parse-response json-resp 200)))
  (check (cdr (assoc 'role parsed)) => "assistant")
  (check (cdr (assoc 'stopReason parsed)) => "stop")
  (let ((content (cdr (assoc 'content parsed))))
    (check (cdr (assoc 'text (car content))) => "Done!")))

;; 工具调用 input 为空对象 {} 时解析为 '()
(let* ((json-resp '(("id" . "msg_tc1")
                    ("type" . "message")
                    ("role" . "assistant")
                    ("content" . #((("type" . "tool_use")
                                    ("id" . "call_1")
                                    ("name" . "ls")
                                    ("input" . (())))))
                    ("stop_reason" . "tool_use")
                    ("usage" . (("input_tokens" . 10) ("output_tokens" . 5)))))
       (parsed (anthropic-parse-response json-resp 200)))
  (check (cdr (assoc 'stopReason parsed)) => "toolUse")
  (let* ((content (cdr (assoc 'content parsed)))
         (blk (car content)))
    (check (pi-tool-call-block? blk) => #t)
    (check (pi-tool-call-block-arguments blk) => '())))

;; 工具调用 input 为 JSON 字符串时反序列化为 alist
(let* ((json-resp '(("id" . "msg_tc2")
                    ("type" . "message")
                    ("role" . "assistant")
                    ("content" . #((("type" . "tool_use")
                                    ("id" . "call_2")
                                    ("name" . "bash")
                                    ("input" . "{\"command\": \"pwd\"}"))))
                    ("stop_reason" . "tool_use")
                    ("usage" . (("input_tokens" . 10) ("output_tokens" . 5)))))
       (parsed (anthropic-parse-response json-resp 200)))
  (check (cdr (assoc 'stopReason parsed)) => "toolUse")
  (let* ((content (cdr (assoc 'content parsed)))
         (blk (car content)))
    (check (pi-tool-call-block? blk) => #t)
    (check (cdr (assoc "command" (pi-tool-call-block-arguments blk))) => "pwd")))

;; API 返回 error 对象
(let* ((json-resp '(("type" . "error")
                    ("error" . (("type" . "invalid_request_error")
                                ("message" . "model not found")))))
       (parsed (anthropic-parse-response json-resp 404)))
  (check (cdr (assoc 'stopReason parsed)) => "error")
  (check (cdr (assoc 'errorMessage parsed)) => "model not found"))

;; 非 JSON 对象响应
(let ((parsed (anthropic-parse-response "not-an-object" 200)))
  (check (cdr (assoc 'stopReason parsed)) => "error")
  (check (cdr (assoc 'errorMessage parsed)) => "Invalid JSON response from Anthropic API"))

(check-report)
