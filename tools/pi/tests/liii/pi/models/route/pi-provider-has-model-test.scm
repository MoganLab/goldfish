(import (liii check) (liii pi models))

(check-set-mode! 'report-failed)

;; pi-provider-has-model?
;; 判断指定 provider 的模型目录（models-store 或内置模型）是否包含该模型

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"
  ) ;if
) ;define

(define fixture-store (pi-load-models-store FIXTURE_DIR))

(define fixture-models-json (pi-load-models-json FIXTURE_DIR))

(define fixture-merged
  (pi-merge-models-json-into-store fixture-store fixture-models-json)
) ;define

;; 1. 从 models-store 中命中
(check-true (pi-provider-has-model? "google" "gemini-3.8-flash" fixture-store))
(check-true (pi-provider-has-model? "google" "gemini-2.5-pro" fixture-store))
(check-true (pi-provider-has-model? "anthropic" "claude-3-7-sonnet-20250219" fixture-store)
) ;check-true

;; 2. 从内置模型列表中命中（即使 models-store 为空）
(check-true (pi-provider-has-model? "kimi-coding" "k3" '()))
(check-true (pi-provider-has-model? "kimi-coding" "kimi-for-coding" '()))
(check-true (pi-provider-has-model? "deepseek" "deepseek-v4-pro" '()))
(check-true (pi-provider-has-model? "deepseek" "deepseek-v4-flash" '()))
(check-true (pi-provider-has-model? "minimax" "MiniMax-M3.1-Flash" '()))
(check-true (pi-provider-has-model? "minimax" "MiniMax-M3" '()))
(check-true (pi-provider-has-model? "minimax" (pi-provider-default-model "minimax") '())
) ;check-true
(check-true (pi-provider-has-model? "minimax-cn" "MiniMax-M3.1-Flash" '()))
(check-true (pi-provider-has-model? "minimax-cn"
              (pi-provider-default-model "minimax-cn")
              '()
            ) ;pi-provider-has-model?
) ;check-true
(check-true (pi-provider-has-model? "minimax-cn" "MiniMax-M2.7" '()))
(check-true (pi-provider-has-model? "minimax-cn" "MiniMax-M2.7-highspeed" '()))
(check-true (pi-provider-has-model? "zai-coding-cn" "glm-5.3" '()))

;; 3. 不存在于该 provider 的模型返回 #f
(check-false (pi-provider-has-model? "google" "k3" fixture-store))
(check-false (pi-provider-has-model? "kimi-coding" "gemini-3.8-flash" '()))
(check-false (pi-provider-has-model? "minimax" "gemini-3.8-flash" '()))
(check-false (pi-provider-has-model? "google" "unknown-xyz" fixture-store))

;; 4. models.json 合并后 glm-5.3 系列命中
(check-true (pi-provider-has-model? "glm" "glm-5.3" fixture-merged))
(check-true (pi-provider-has-model? "glm" "glm-5.3-flash" fixture-merged))
(check-true (pi-provider-has-model? "glm" "glm-5-turbo" fixture-merged))
(check-true (pi-provider-has-model? "glm" "glm-4.5-air" fixture-merged))

(check-report)
