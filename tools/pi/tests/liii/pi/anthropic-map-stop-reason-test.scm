(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; anthropic-map-stop-reason
;; 将 Anthropic 响应 stop_reason 映射到 pi agent 标准 stopReason。
(check (anthropic-map-stop-reason "end_turn") => "stop")
(check (anthropic-map-stop-reason "max_tokens") => "length")
(check (anthropic-map-stop-reason "tool_use") => "toolUse")
(check (anthropic-map-stop-reason "refusal") => "error")
(check (anthropic-map-stop-reason "pause_turn") => "stop")
(check (anthropic-map-stop-reason "stop_sequence") => "stop")
(check (anthropic-map-stop-reason "sensitive") => "error")
(check (anthropic-map-stop-reason "unknown") => "error")

(check-report)
