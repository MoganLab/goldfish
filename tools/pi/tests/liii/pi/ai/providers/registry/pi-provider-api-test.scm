(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)

;; pi-provider-api
;; provider 注册表声明的协议实现标识，dispatch 据此路由请求。
(check (pi-provider-api "deepseek") => "anthropic-messages")
(check (pi-provider-api "minimax-cn") => "anthropic-messages")
(check (pi-provider-api "minimax-cn") => "anthropic-messages")
(check (pi-provider-api "google") => "google-generative-ai")
(check (pi-provider-api "kimi-coding") => "openai-completions")
(check (pi-provider-api "zai-coding-cn") => "openai-completions")

;; "zai" 是 "zai-coding-cn" 的路由别名
(check (pi-provider-api "zai") => "openai-completions")

;; 未注册 provider 默认走 google-generative-ai
(check (pi-provider-api "unknown-provider") => "google-generative-ai")

;; symbol 入参与 string 等价
(check (pi-provider-api 'kimi-coding) => "openai-completions")

(check-report)
