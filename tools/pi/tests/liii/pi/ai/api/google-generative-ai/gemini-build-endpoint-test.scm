(import (liii check) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

;; gemini-build-endpoint
;; 构造发往 Gemini generateContent 端的完整 URL。
(check (gemini-build-endpoint #f "gemini-2.0-flash")
  => "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent")

;; 带尾斜杠
(check (gemini-build-endpoint "https://generativelanguage.googleapis.com/v1beta/"
       "gemini-2.0-flash"
       ) ;gemini-build-endpoint
  => "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent")

;; 自定义 base-url
(check (gemini-build-endpoint "https://proxy.example.com/v1" "gemini-2.0-flash")
  => "https://proxy.example.com/v1/models/gemini-2.0-flash:generateContent")

;; gemini-build-stream-endpoint
;; 构造发往 Gemini streamGenerateContent 端的完整 URL（SSE）。
(check (gemini-build-stream-endpoint #f "gemini-2.0-flash")
  => "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse")

;; 带尾斜杠
(check (gemini-build-stream-endpoint "https://generativelanguage.googleapis.com/v1beta/"
       "gemini-2.0-flash"
       ) ;gemini-build-stream-endpoint
  => "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse")

(check-report)
