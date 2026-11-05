;; openai-build-headers
;; 构造发往 OpenAI Chat Completions API 的 HTTP 请求头。
;;
;; 语法
;; ----
;; (openai-build-headers api-key)
;;
;; 参数
;; ----
;; api-key : string? | #f
;; API 访问密钥。
;;
;; 返回值
;; ----
;; alist
;; 包含 Content-Type、Authorization 与 User-Agent 的请求头列表。

(import (liii check)
  (liii pi ai api openai-completions request)
) ;import

(check-set-mode! 'report-failed)

;; 正常密钥
(let ((headers (openai-build-headers "sk-test-123")))
  (check (cdr (assoc "Content-Type" headers)) => "application/json")
  (check (cdr (assoc "Authorization" headers)) => "Bearer sk-test-123")
  (check (cdr (assoc "User-Agent" headers)) => "Goldfish-Pi/1.0")
) ;let

;; 空密钥或 #f
(let ((headers (openai-build-headers #f)))
  (check (cdr (assoc "Authorization" headers)) => "Bearer ")
) ;let

(check-report)
