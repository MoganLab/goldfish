;; gemini-build-endpoint
;; 构造发往 Gemini generateContent API 端的完整 URL。
;;
;; 语法
;; ----
;; (gemini-build-endpoint base-url model-id)
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
;; 形如 <base-url>/models/<model-id>:generateContent 的请求 URL。

(import (liii check) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

(check (gemini-build-endpoint #f "gemini-2.0-flash")
  =>
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"
) ;check

(check (gemini-build-endpoint "https://generativelanguage.googleapis.com/v1beta/"
         "gemini-2.0-flash"
       ) ;gemini-build-endpoint
  =>
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"
) ;check

(check (gemini-build-endpoint "https://proxy.example.com/v1" "gemini-2.0-flash")
  =>
  "https://proxy.example.com/v1/models/gemini-2.0-flash:generateContent"
) ;check

(check-report)
