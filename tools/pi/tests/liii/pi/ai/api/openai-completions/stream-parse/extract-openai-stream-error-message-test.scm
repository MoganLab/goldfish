;; extract-openai-stream-error-message
;; 从 OpenAI 错误响应体中提取错误信息。
;;
;; 语法
;; ----
;; (extract-openai-stream-error-message body)
;;
;; 参数
;; ----
;; body : string
;; 响应体原文，期望为 JSON 格式。
;;
;; 返回值
;; ----
;; string | #f
;; 错误信息；无法解析或没有 error 字段时返回 #f。

(import (liii check)
  (liii pi ai api openai-completions stream-parse)
) ;import

(check-set-mode! 'report-failed)

;; error 对象中的 message
(check (extract-openai-stream-error-message "{\"error\":{\"message\":\"invalid key\"}}")
  => "invalid key")

;; error 为字符串
(check (extract-openai-stream-error-message "{\"error\":\"boom\"}") => "boom")

;; 没有 error 字段
(check (extract-openai-stream-error-message "{}") => #f)

;; 非 JSON 文本
(check (extract-openai-stream-error-message "not-a-json") => #f)
(check (extract-openai-stream-error-message "") => #f)

(check-report)
