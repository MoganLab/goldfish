(import (liii check) (liii pi models))

(check-set-mode! 'report-failed)

;; pi-provider-default-model
;; 返回指定 provider 的默认模型标识

(check (pi-provider-default-model "google") => "gemini-3.8-flash")
(check (pi-provider-default-model "anthropic") => "claude-opus-4-8")
(check (pi-provider-default-model "kimi-coding") => "kimi-for-coding")
(check (pi-provider-default-model "glm") => "glm-5.3")
(check (pi-provider-default-model "deepseek") => "deepseek-v4-pro")
(check (pi-provider-default-model "minimax") => "MiniMax-M3")
(check (pi-provider-default-model "minimax-cn") => "MiniMax-M3")
(check (pi-provider-default-model "zai-coding-cn") => "glm-5.3")
(check (pi-provider-default-model "other") => "gemini-3.8-flash")

(check-report)
