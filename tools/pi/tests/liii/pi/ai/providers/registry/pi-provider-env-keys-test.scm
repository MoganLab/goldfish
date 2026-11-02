;; pi-provider-env-keys
;; 获取服务商依次尝试的 API Key 环境变量名称列表。
;;
;; 语法
;; ----
;; (pi-provider-env-keys provider)
;;
;; 参数
;; ----
;; provider : string? 或 symbol?
;; 服务商标识符。
;;
;; 返回值
;; ----
;; list?
;; 环境变量名字符串列表；若未注册则返回空列表。

(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)
;; anthropic / glm provider 已移除，未注册返回空表
(check (pi-provider-env-keys "anthropic") => '())
(check (pi-provider-env-keys "glm") => '())
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
