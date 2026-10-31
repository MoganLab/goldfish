(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)

;; 各 provider 依次尝试的 API key 环境变量
(check (pi-provider-env-keys "anthropic")
  =>
  '("ANTHROPIC_API_KEY" "ANTHROPIC_AUTH_TOKEN")
) ;check
(check (pi-provider-env-keys "glm")
  =>
  '("ANTHROPIC_AUTH_TOKEN" "GLM_API_KEY" "ANTHROPIC_API_KEY")
) ;check
(check (pi-provider-env-keys "deepseek") => '("DEEPSEEK_API_KEY"))
(check (pi-provider-env-keys "google") => '("GEMINI_API_KEY"))
(check (pi-provider-env-keys "kimi-coding") => '("KIMI_API_KEY"))
;; minimax 国际版 provider 已移除，未注册返回空表
(check (pi-provider-env-keys "minimax") => '())
(check (pi-provider-env-keys "minimax-cn")
  =>
  '("MINIMAX_CN_API_KEY" "MINIMAX_API_KEY")
) ;check
(check (pi-provider-env-keys "zai-coding-cn")
  =>
  '("ZAI_CODING_CN_API_KEY" "ZAI_API_KEY" "GLM_API_KEY" "ZHIPU_API_KEY")
) ;check

;; 未注册 provider 返回空表
(check (pi-provider-env-keys "unknown-provider") => '())

(check-report)
