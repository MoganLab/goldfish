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

;; 携带附加请求头
(let ((headers (openai-build-headers "sk-test" (list (cons "x-opencode-session" "sess-42")))))
  (check (cdr (assoc "x-opencode-session" headers)) => "sess-42")
) ;let

;; openai-opencode-extra-headers 自动生成或复用 session
(let ((hdrs (openai-opencode-extra-headers "https://opencode.ai/zen/go/v1" "opencode" "custom-session")))
  (check (cdr (assoc "x-opencode-session" hdrs)) => "custom-session")
) ;let
(let ((hdrs (openai-opencode-extra-headers "https://opencode.ai/zen/go/v1" "opencode" #f)))
  (check-true (string? (cdr (assoc "x-opencode-session" hdrs))))
) ;let
(let ((hdrs (openai-opencode-extra-headers "https://api.kimi.com/coding/v1" "kimi-coding" #f)))
  (check hdrs => '())
) ;let

(check-report)
