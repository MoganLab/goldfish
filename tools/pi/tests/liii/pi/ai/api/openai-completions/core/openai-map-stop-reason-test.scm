(import (liii check) (liii pi ai api openai-completions))

(check-set-mode! 'report-failed)

;; openai-map-stop-reason
;; 将 Kimi/OpenAI 响应 finish_reason 映射到 pi agent 标准 stopReason。
(check (openai-map-stop-reason "stop") => "stop")
(check (openai-map-stop-reason "length") => "length")
(check (openai-map-stop-reason "tool_calls") => "toolUse")
(check (openai-map-stop-reason "content_filter") => "error")
(check (openai-map-stop-reason "unknown") => "error")

(check-report)
