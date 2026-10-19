(import (liii check)
        (liii pi models))

(check-set-mode! 'report-failed)

;; pi-provider-default-model
;; 返回指定 provider 的默认模型标识

(check (pi-provider-default-model "google") => "gemini-3.8-flash")
(check (pi-provider-default-model "anthropic") => "claude-3-7-sonnet-20250219")
(check (pi-provider-default-model "kimi-coding") => "kimi-for-coding")
(check (pi-provider-default-model "glm") => "glm-4-plus")
(check (pi-provider-default-model "deepseek") => "deepseek-chat")
(check (pi-provider-default-model "other") => "gemini-3.8-flash")

(check-report)
