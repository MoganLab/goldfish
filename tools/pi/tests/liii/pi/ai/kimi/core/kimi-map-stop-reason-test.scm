(import (liii check) (liii pi ai kimi))

(check-set-mode! 'report-failed)

;; kimi-map-stop-reason
;; 将 Kimi/OpenAI 响应 finish_reason 映射到 pi agent 标准 stopReason。
(check (kimi-map-stop-reason "stop") => "stop")
(check (kimi-map-stop-reason "length") => "length")
(check (kimi-map-stop-reason "tool_calls") => "toolUse")
(check (kimi-map-stop-reason "content_filter") => "error")
(check (kimi-map-stop-reason "unknown") => "error")

(check-report)
