(import (liii check) (liii pi ai anthropic) (liii pi ai event))

(check-set-mode! 'report-failed)

;; anthropic-build-request-body
;; 组装发送给 Anthropic API 的 JSON 请求体。
(let* ((body (anthropic-build-request-body "claude-3-7-sonnet-20250219"
                                          #((("role" . "user") ("content" . "hi")))
                                          "You are a helpful assistant"
                                          '()
                                          4096)))
  (check (cdr (assoc "model" body)) => "claude-3-7-sonnet-20250219")
  (check (cdr (assoc "max_tokens" body)) => 4096)
  (check (cdr (assoc "system" body)) => "You are a helpful assistant")
  (check (vector-length (cdr (assoc "messages" body))) => 1)
  ;; 无 tools 时（空列表），请求体中不含 tools 字段
  (check (assoc "tools" body) => #f))

(check-report)
