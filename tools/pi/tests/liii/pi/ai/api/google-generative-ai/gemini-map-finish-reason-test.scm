(import (liii check) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

;; gemini-map-finish-reason
;; 映射 Gemini 的 finishReason 到 pi agent 的 stopReason。
(check (gemini-map-finish-reason "STOP" #f) => "stop")
(check (gemini-map-finish-reason "STOP" #t) => "toolUse")
(check (gemini-map-finish-reason "MAX_TOKENS" #f) => "length")
(check (gemini-map-finish-reason "SAFETY" #f) => "error")
(check (gemini-map-finish-reason "OTHER" #f) => "error")

(check-report)
