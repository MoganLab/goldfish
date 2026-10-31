(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)

;; pi-provider-base-url
;; 各 provider 的默认端点，与 models 目录解析出的默认 baseUrl 一致。
(check (pi-provider-base-url "anthropic") => "https://api.anthropic.com")
(check (pi-provider-base-url "glm") => "https://open.bigmodel.cn/api/anthropic")
(check (pi-provider-base-url "deepseek")
  =>
  "https://api.deepseek.com/anthropic"
) ;check
(check (pi-provider-base-url "google")
  =>
  "https://generativelanguage.googleapis.com/v1beta"
) ;check
(check (pi-provider-base-url "kimi-coding") => "https://api.kimi.com/coding/v1")
(check (pi-provider-base-url "minimax") => "https://api.minimax.io/anthropic")
(check (pi-provider-base-url "minimax-cn")
  =>
  "https://api.minimaxi.com/anthropic"
) ;check
(check (pi-provider-base-url "zai-coding-cn")
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4"
) ;check

;; "zai" 别名共享配置
(check (pi-provider-base-url "zai")
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4"
) ;check

;; 未注册 provider 无默认端点
(check (pi-provider-base-url "unknown-provider") => #f)

(check-report)
