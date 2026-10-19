(import (liii check)
        (liii pi models builtin))

(check-set-mode! 'report-failed)

;; PI_PROVIDER_BUILTIN_MODELS
;; 各 provider 的默认内置模型定义列表

(check-true (pair? PI_PROVIDER_BUILTIN_MODELS))
(check-true (pair? (assoc "google" PI_PROVIDER_BUILTIN_MODELS)))
(check-true (pair? (assoc "kimi-coding" PI_PROVIDER_BUILTIN_MODELS)))
(check-true (pair? (assoc "anthropic" PI_PROVIDER_BUILTIN_MODELS)))
(check-true (pair? (assoc "glm" PI_PROVIDER_BUILTIN_MODELS)))
(check-true (pair? (assoc "deepseek" PI_PROVIDER_BUILTIN_MODELS)))
(check-true (pair? (assoc "minimax" PI_PROVIDER_BUILTIN_MODELS)))
(check-true (pair? (assoc "minimax-cn" PI_PROVIDER_BUILTIN_MODELS)))

(check-report)
