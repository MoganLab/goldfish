;; extract-stream-error-message
;; 从 Gemini 错误响应体中提取错误信息。
;;
;; 语法
;; ----
;; (extract-stream-error-message body)
;;
;; 参数
;; ----
;; body : string
;; 响应体原文，期望为 JSON 对象。
;;
;; 返回值
;; ----
;; string | #f
;; 错误信息；无法解析或没有 error 字段时返回 #f。

(import (liii check) (liii pi ai api google-generative-ai stream-parse))

(check-set-mode! 'report-failed)

;; error 对象中的 message
(check (extract-stream-error-message "{\"error\":{\"message\":\"quota exceeded\"}}")
  =>
  "quota exceeded"
) ;check

;; error 为字符串
(check (extract-stream-error-message "{\"error\":\"boom\"}") => "boom")

;; 没有 error 字段
(check (extract-stream-error-message "{}") => #f)

;; 非 JSON 文本
(check (extract-stream-error-message "not-a-json") => #f)

;; 空字符串
(check (extract-stream-error-message "") => #f)

(check-report)
