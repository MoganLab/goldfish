(import (liii check) (liii pi deepseek))

(check-set-mode! 'report-failed)

;; deepseek-map-stop-reason
;; 将 DeepSeek/Anthropic 兼容网关 stop_reason 映射到 pi agent 标准 stopReason。
(check (deepseek-map-stop-reason "end_turn") => "stop")
(check (deepseek-map-stop-reason "stop_sequence") => "stop")
(check (deepseek-map-stop-reason "max_tokens") => "length")
(check (deepseek-map-stop-reason "tool_use") => "toolUse")
(check (deepseek-map-stop-reason "unknown") => "error")

(check-report)
