(import (liii check)
        (liii pi models))

(check-set-mode! 'report-failed)

;; pi-route-provider-and-model
;; 统一解析 CLI 参数、环境变量与配置，确定生效的 provider 与 model-id

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/default")
      "tests/fixtures/default"
      "tools/pi/tests/fixtures/default"))

(define fixture-store (pi-load-models-store FIXTURE_DIR))
(define fixture-merged
  (pi-merge-models-json-into-store fixture-store (pi-load-models-json FIXTURE_DIR)))
(define empty-settings '())
(define default-settings '(("default-provider" . "google") ("default-model" . "gemini-3.8-flash")))

;; 1. 显式指定 --provider 时以显式为准
(let-values (((p m) (pi-route-provider-and-model "google" "k3" default-settings fixture-store '())))
  (check p => "google")
  (check m => "k3"))

;; 2. 未指定 --provider 时，按 --model 跨 provider 自动路由
;; 2.1 k3 自动路由至 kimi-coding
(let-values (((p m) (pi-route-provider-and-model #f "k3" default-settings fixture-store '())))
  (check p => "kimi-coding")
  (check m => "k3"))

;; 2.2 glm-4-plus 自动路由至 glm
(let-values (((p m) (pi-route-provider-and-model #f "glm-4-plus" default-settings fixture-store '())))
  (check p => "glm")
  (check m => "glm-4-plus"))

;; 2.3 deepseek-chat 自动路由至 deepseek
(let-values (((p m) (pi-route-provider-and-model #f "deepseek-chat" default-settings fixture-store '())))
  (check p => "deepseek")
  (check m => "deepseek-chat"))

;; 2.3.1 glm-5.3 / glm-5.3-flash 自动路由至 glm（models.json 合并目录，任务 028）
(let-values (((p m) (pi-route-provider-and-model #f "glm-5.3" default-settings fixture-merged '())))
  (check p => "glm")
  (check m => "glm-5.3"))

(let-values (((p m) (pi-route-provider-and-model #f "glm-5.3-flash" default-settings fixture-merged '())))
  (check p => "glm")
  (check m => "glm-5.3-flash"))

;; 2.4 未命中任何 provider 时兜底至当前 default-provider 并保留该模型
(let-values (((p m) (pi-route-provider-and-model #f "custom-unknown-model" default-settings fixture-store '())))
  (check p => "google")
  (check m => "custom-unknown-model"))

;; 3. 未显式指定 provider 且未显式指定 model 时，取 settings 或系统默认
(let-values (((p m) (pi-route-provider-and-model #f #f default-settings fixture-store '())))
  (check p => "google")
  (check m => "gemini-3.8-flash"))

;; 4. 环境变量 GF_PI_MODEL 显式模型生效并触发跨 provider 路由
(let-values (((p m) (pi-route-provider-and-model #f #f default-settings fixture-store '(("GF_PI_MODEL" . "k3")))))
  (check p => "kimi-coding")
  (check m => "k3"))

(check-report)
