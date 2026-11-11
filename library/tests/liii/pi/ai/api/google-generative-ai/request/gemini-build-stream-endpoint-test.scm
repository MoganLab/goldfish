;; gemini-build-stream-endpoint
;; 构造发往 Gemini streamGenerateContent API 端的完整 SSE URL。
;;
;; 语法
;; ----
;; (gemini-build-stream-endpoint base-url model-id)
;;
;; 参数
;; ----
;; base-url : string? | #f
;; 服务商基础地址；缺省或 #f 时使用官方地址
;; https://generativelanguage.googleapis.com/v1beta。
;; model-id : string
;; 模型 id。
;;
;; 返回值
;; ----
;; string
;; 形如 <base-url>/models/<model-id>:streamGenerateContent?alt=sse 的请求 URL。

(import (liii check) (liii pi ai api google-generative-ai request))

(check-set-mode! 'report-failed)

;; 缺省基础地址
(check (gemini-build-stream-endpoint #f "gemini-2.0-flash")
  =>
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse"
) ;check

;; 带尾斜杠的基础地址
(check (gemini-build-stream-endpoint "https://generativelanguage.googleapis.com/v1beta/"
         "gemini-2.0-flash"
       ) ;gemini-build-stream-endpoint
  =>
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse"
) ;check

(check-report)
