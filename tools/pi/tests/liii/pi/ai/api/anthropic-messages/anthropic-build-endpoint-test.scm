(import (liii check) (liii pi ai api anthropic-messages))

(check-set-mode! 'report-failed)

;; anthropic-build-endpoint
;; 构造发往 Anthropic Messages API 端的完整 URL。
(check (anthropic-build-endpoint "https://api.anthropic.com")
  => "https://api.anthropic.com/v1/messages")

;; 缺省值
(check (anthropic-build-endpoint #f)
  => "https://api.anthropic.com/v1/messages")

;; 带尾斜杠
(check (anthropic-build-endpoint "https://api.anthropic.com/")
  => "https://api.anthropic.com/v1/messages")

;; glm 的 Anthropic 兼容端点
(check (anthropic-build-endpoint "https://open.bigmodel.cn/api/anthropic")
  => "https://open.bigmodel.cn/api/anthropic/v1/messages")

;; deepseek 的 Anthropic 兼容端点
(check (anthropic-build-endpoint "https://api.deepseek.com/anthropic")
  => "https://api.deepseek.com/anthropic/v1/messages")

(check-report)
