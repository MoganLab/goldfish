(import (liii check) (liii pi ai models))

(check-set-mode! 'report-failed)

;; 内置目录（含 models 包裹结构），供可用性判定使用
(define BUILTIN-CATALOG (pi-builtin-augment-all '()))

;; pi-provider-has-model?
;; 判断指定 provider 的模型目录是否包含该模型（可用性以目录为准）

;; 样本目录：自定义模型 + 内置目录
(define custom-catalog
  (list
    (cons "google" (list (cons "models" (list (list (cons "id" "gemini-3.8-flash"))
                                              (list (cons "id" "gemini-2.5-pro"))))))
    (cons "zai-coding-cn"
      (list (cons "models" (list (list (cons "id" "glm-5.3"))
                                 (list (cons "id" "glm-5.3-flash"))
                                 (list (cons "id" "glm-5-turbo"))
                                 (list (cons "id" "glm-4.5-air"))))))))

(define fixture-merged (pi-builtin-augment-all custom-catalog))

;; 1. 自定义目录命中
(check-true (pi-provider-has-model? "google" "gemini-3.8-flash" custom-catalog))
(check-true (pi-provider-has-model? "google" "gemini-2.5-pro" custom-catalog))
(check-true (pi-provider-has-model? "deepseek" "deepseek-v4-pro" BUILTIN-CATALOG)
) ;check-true

;; 2. 从内置模型列表中命中（即使 models-store 为空）
(check-true (pi-provider-has-model? "kimi-coding" "k3" BUILTIN-CATALOG))
(check-true (pi-provider-has-model? "kimi-coding" "kimi-for-coding" BUILTIN-CATALOG))
(check-true (pi-provider-has-model? "deepseek" "deepseek-v4-pro" BUILTIN-CATALOG))
(check-true (pi-provider-has-model? "deepseek" "deepseek-v4-flash" BUILTIN-CATALOG))
(check-true (pi-provider-has-model? "minimax-cn" "MiniMax-M3.1-Flash" BUILTIN-CATALOG))
(check-true (pi-provider-has-model? "minimax-cn" "MiniMax-M3" BUILTIN-CATALOG))
(check-true (pi-provider-has-model? "minimax-cn" (pi-provider-default-model "minimax-cn") BUILTIN-CATALOG)
) ;check-true
(check-true (pi-provider-has-model? "minimax-cn" "MiniMax-M3.1-Flash" BUILTIN-CATALOG))
(check-true (pi-provider-has-model? "minimax-cn"
              (pi-provider-default-model "minimax-cn")
              BUILTIN-CATALOG
            ) ;pi-provider-has-model?
) ;check-true
(check-true (pi-provider-has-model? "minimax-cn" "MiniMax-M2.7" BUILTIN-CATALOG))
(check-true (pi-provider-has-model? "minimax-cn" "MiniMax-M2.7-highspeed" BUILTIN-CATALOG))
(check-true (pi-provider-has-model? "zai-coding-cn" "glm-5.3" BUILTIN-CATALOG))

;; 3. 不存在于该 provider 的模型返回 #f
(check-false (pi-provider-has-model? "google" "k3" custom-catalog))
(check-false (pi-provider-has-model? "kimi-coding" "gemini-3.8-flash" BUILTIN-CATALOG))
(check-false (pi-provider-has-model? "minimax-cn" "gemini-3.8-flash" BUILTIN-CATALOG))
(check-false (pi-provider-has-model? "google" "unknown-xyz" custom-catalog))

;; 4. 自定义目录中的 glm 系列命中（provider 键为 zai-coding-cn）
(check-true (pi-provider-has-model? "zai-coding-cn" "glm-5.3" fixture-merged))
(check-true (pi-provider-has-model? "zai-coding-cn" "glm-5.3-flash" fixture-merged))
(check-true (pi-provider-has-model? "zai-coding-cn" "glm-5-turbo" fixture-merged))
(check-true (pi-provider-has-model? "zai-coding-cn" "glm-4.5-air" fixture-merged))

(check-report)
