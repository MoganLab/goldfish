;; gemini-normalize-tool-call-id
;; 归一化工具调用 ID 符号。
;;
;; 语法
;; ----
;; (gemini-normalize-tool-call-id id)
;;
;; 参数
;; ----
;; id : string? | any
;; 原始工具调用 id；非字符串时返回缺省 id。
;;
;; 返回值
;; ----
;; string
;; 规范化后的工具调用 id。

(import (liii check) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

(check (gemini-normalize-tool-call-id "valid_id-123") => "valid_id-123")
(check (gemini-normalize-tool-call-id "invalid.id:special!chars") => "invalid_id_special_chars")

(check-report)
