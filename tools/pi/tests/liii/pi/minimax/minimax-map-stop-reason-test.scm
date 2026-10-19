(import (liii check) (liii pi minimax))

(check-set-mode! 'report-failed)

;; minimax-map-stop-reason
;; 将 MiniMax / Anthropic 兼容网关 stop_reason 映射到 pi agent 标准 stopReason。
(check (minimax-map-stop-reason "end_turn") => "stop")
(check (minimax-map-stop-reason "stop_sequence") => "stop")
(check (minimax-map-stop-reason "max_tokens") => "length")
(check (minimax-map-stop-reason "tool_use") => "toolUse")
(check (minimax-map-stop-reason "unknown") => "error")

(check-report)
