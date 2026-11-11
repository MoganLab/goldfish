;; gemini-normalize-tool-call-id
;; 规范化 Gemini 工具调用 id，保证只含字母、数字、下划线或连字符且长度不超过 64。
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

(import (liii check) (liii pi ai api google-generative-ai tool-call-id))

(check-set-mode! 'report-failed)

;; 合法字符保持不变
(check (gemini-normalize-tool-call-id "call_123") => "call_123")
(check (gemini-normalize-tool-call-id "call-123") => "call-123")

;; 非法字符替换为下划线
(check (gemini-normalize-tool-call-id "call/123") => "call_123")
(check (gemini-normalize-tool-call-id "call 123!") => "call_123_")

;; 非字符串返回缺省 id
(check (gemini-normalize-tool-call-id #f) => "tool_call_0")
(check (gemini-normalize-tool-call-id 42) => "tool_call_0")

;; 超长 id 被截断到 64 个字符
(let ((long-id (string-append "a" (make-string 100 #\b))))
  (check (string-length (gemini-normalize-tool-call-id long-id)) => 64)
) ;let

(check-report)
