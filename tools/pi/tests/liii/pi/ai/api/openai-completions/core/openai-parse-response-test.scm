(import (liii check) (liii pi ai event) (liii pi ai api openai-completions) (liii pi ai message))

(check-set-mode! 'report-failed)

;; openai-parse-response
;; 将 OpenAI Chat Completions JSON 响应解析为内部 assistant message。
;;
;; 语法
;; ----
;; (openai-parse-response json-data status-code)
;;
;; 参数
;; ----
;; json-data : alist
;; OpenAI API 响应对象。
;; status-code : integer
;; HTTP 响应状态码。
;;
;; 返回值
;; ----
;; pi-assistant-message
;; 包含 content、stopReason、usage 和可选 errorMessage 的消息 record。
;;
;; 说明
;; ----
;; 文本和工具调用转换为内部 content block，并保持原始顺序。
;; API 错误响应映射为 stopReason=error。
;;
;; 示例
;; ----
;; (pi-assistant-message-stop-reason (openai-parse-response response 200)) => "stop"

;; 正常文本响应解析
(let* ((json-data '(("id" . "chatcmpl-1")
                    ("choices" . #((("index" . 0)
                                    ("message" . (("role" . "assistant")
                                                  ("content" . "PONG")))
                                    ("finish_reason" . "stop"))))
                    ("usage" . (("prompt_tokens" . 10)
                                ("completion_tokens" . 5)
                                ("total_tokens" . 15)))))
       (resp (openai-parse-response json-data 200)))
  (check (pi-assistant-message? resp) => #t)
  (check (pi-assistant-message-stop-reason resp) => "stop")
  (let ((content (pi-assistant-message-content resp)))
    (check (length content) => 1)
    (check (pi-text-block-text (car content)) => "PONG")))

;; 工具调用响应解析
(let* ((json-data '(("id" . "chatcmpl-2")
                    ("choices" . #((("index" . 0)
                                    ("message" . (("role" . "assistant")
                                                  ("content" . "")
                                                  ("tool_calls" . #((("id" . "call_1")
                                                                     ("type" . "function")
                                                                     ("function" . (("name" . "bash")
                                                                                    ("arguments" . "{\"command\":\"ls\"}"))))))))
                                    ("finish_reason" . "tool_calls"))))
                    ("usage" . (("prompt_tokens" . 20)
                                ("completion_tokens" . 10)
                                ("total_tokens" . 30)))))
       (resp (openai-parse-response json-data 200)))
  (check (pi-assistant-message-stop-reason resp) => "toolUse")
  (let ((content (pi-assistant-message-content resp)))
    (check (length content) => 1)
    (let ((tc (car content)))
      (check (pi-tool-call-block? tc) => #t)
      (check (pi-tool-call-block-id tc) => "call_1")
      (check (pi-tool-call-block-name tc) => "bash")
      (check (cdr (assoc "command" (pi-tool-call-block-arguments tc))) => "ls"))))

;; 错误响应解析
(let* ((json-data '(("error" . (("message" . "Invalid API key")
                                ("type" . "invalid_request_error")))))
       (resp (openai-parse-response json-data 401)))
  (check (pi-assistant-message-stop-reason resp) => "error")
  (check (pi-assistant-message-error-message resp) => "Invalid API key"))

;; 文本与多个工具调用保持原始顺序
(let* ((json-data '(("choices" . #((("message" . (("role" . "assistant")
                                                   ("content" . "Working")
                                                   ("tool_calls" . #((("id" . "call_1")
                                                                      ("function" . (("name" . "read")
                                                                                     ("arguments" . "{}"))))
                                                                     (("id" . "call_2")
                                                                      ("function" . (("name" . "bash")
                                                                                     ("arguments" . "{}"))))))))
                                    ("finish_reason" . "tool_calls"))))))
       (resp (openai-parse-response json-data 200))
       (content (pi-assistant-message-content resp)))
  (check (length content) => 3)
  (check (pi-text-block-text (car content)) => "Working")
  (check (pi-tool-call-block-id (cadr content)) => "call_1")
  (check (pi-tool-call-block-id (caddr content)) => "call_2"))

(check-report)
