(import (liii check)
  (liii pi ai providers registry)
  (liii pi ai models builtin)
) ;import

(check-set-mode! 'report-failed)

;; pi-provider-builtin-models
;; registry 提供的内置模型目录与 models/builtin 聚合接口同源。

(define (has-model? models id)
  (if (null? models)
    #f
    (if
      (equal? (cdr (assoc "id" (car models))) id)
      #t
      (has-model? (cdr models) id)
    ) ;if
  ) ;if
) ;define

(check-true (has-model? (pi-provider-builtin-models "kimi-coding") "kimi-for-coding")
) ;check-true
(check-true (has-model? (pi-provider-builtin-models "zai-coding-cn") "glm-4.7"))
(check-true (has-model? (pi-provider-builtin-models "deepseek") "deepseek-v4-pro")
) ;check-true
(check-true (has-model? (pi-provider-builtin-models "anthropic") "claude-fable-5")
) ;check-true

;; PI_PROVIDER_BUILTIN_MODELS 两处导出一致（均为 id -> models 的 alist）
(check-true (pair? (assoc "kimi-coding" PI_PROVIDER_BUILTIN_MODELS)))

;; "zai" 别名条目不在内置模型目录中（目录按正式 id 组织）
(check (pi-provider-builtin-models "zai") => '())

;; glm 无内置模型目录
(check (pi-provider-builtin-models "glm") => '())

;; 支持的 provider 列表
(check PI_SUPPORTED_PROVIDERS
  =>
  '("google" "anthropic" "kimi-coding" "glm" "deepseek" "minimax" "minimax-cn"
    "zai-coding-cn")
) ;check

(check-report)
