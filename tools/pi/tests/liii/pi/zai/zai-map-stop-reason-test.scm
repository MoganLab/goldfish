(import (liii check) (liii pi zai))

(check-set-mode! 'report-failed)

;; zai-map-stop-reason
;; 将 OpenAI 风格的 finish_reason 映射到 pi agent 标准 stopReason。
(check (zai-map-stop-reason "stop") => "stop")
(check (zai-map-stop-reason "length") => "length")
(check (zai-map-stop-reason "tool_calls") => "toolUse")
(check (zai-map-stop-reason "content_filter") => "error")
(check (zai-map-stop-reason "unknown") => "error")

(check-report)
