;; pi-provider-builtin-models
;; 获取服务商的内置模型目录列表。
;;
;; 语法
;; ----
;; (pi-provider-builtin-models provider)
;;
;; 参数
;; ----
;; provider : string? 或 symbol?
;; 服务商标识符。
;;
;; 返回值
;; ----
;; list?
;; 该服务商支持的内置模型配置列表。

(import (liii check)
  (liii pi ai providers registry)
  (liii pi ai models builtin)
) ;import

(check-set-mode! 'report-failed)

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

;; PI_PROVIDER_BUILTIN_MODELS 两处导出一致（均为 id -> models 的 alist）
(check-true (pair? (assoc "kimi-coding" PI_PROVIDER_BUILTIN_MODELS)))

;; "zai" 别名条目不在内置模型目录中（目录按正式 id 组织）
(check (pi-provider-builtin-models "zai") => '())

;; anthropic / glm provider 已移除，无内置模型目录
(check (pi-provider-builtin-models "glm") => '())
(check (pi-provider-builtin-models "anthropic") => '())

;; 支持的 provider 列表
(check PI_SUPPORTED_PROVIDERS
  =>
  '("google" "kimi-coding" "deepseek" "minimax-cn" "zai-coding-cn")
) ;check

(check-report)
