(import (liii check) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

;; gemini-generate-tool-call-id
;; 自动生成合规的工具调用 ID。
(check (gemini-generate-tool-call-id "bash" 1700000000 1) => "bash_1700000000_1")

(check-report)
