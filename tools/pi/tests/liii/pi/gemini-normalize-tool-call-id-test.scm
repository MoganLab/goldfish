(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; gemini-normalize-tool-call-id
;; 归一化工具调用 ID 符号。
(check (gemini-normalize-tool-call-id "valid_id-123") => "valid_id-123")
(check (gemini-normalize-tool-call-id "invalid.id:special!chars") => "invalid_id_special_chars")

(check-report)
