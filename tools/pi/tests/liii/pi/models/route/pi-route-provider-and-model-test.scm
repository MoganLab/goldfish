(import (liii check)
        (liii string)
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

;; 1. 显式指定 --provider 且模型属于该 provider 时正常命中
(let-values (((p m) (pi-route-provider-and-model "google" "gemini-3.8-flash" default-settings fixture-store '())))
  (check p => "google")
  (check m => "gemini-3.8-flash"))

(let-values (((p m) (pi-route-provider-and-model "kimi-coding" "k3" default-settings fixture-store '())))
  (check p => "kimi-coding")
  (check m => "k3"))

(let-values (((p m) (pi-route-provider-and-model "deepseek" "deepseek-v4-flash" default-settings fixture-merged '())))
  (check p => "deepseek")
  (check m => "deepseek-v4-flash"))

(let-values (((p m) (pi-route-provider-and-model "minimax" "MiniMax-M3.1-Flash" default-settings fixture-store '())))
  (check p => "minimax")
  (check m => "MiniMax-M3.1-Flash"))

(let-values (((p m) (pi-route-provider-and-model "minimax-cn" "MiniMax-M2.7" default-settings fixture-store '())))
  (check p => "minimax-cn")
  (check m => "MiniMax-M2.7"))

;; 1.1 显式 provider 与模型归属不匹配时报 pi-config-error（不允许 fallback 继续）
(check-catch 'pi-config-error
  (pi-route-provider-and-model "google" "k3" default-settings fixture-store '()))

(check-catch 'pi-config-error
  (pi-route-provider-and-model "kimi-coding" "deepseek-v4-flash" default-settings fixture-merged '()))

;; 1.2 显式 provider 下模型在 models-store 查无记录时报 pi-config-error
(check-catch 'pi-config-error
  (pi-route-provider-and-model "google" "non-existent-model-xyz" default-settings fixture-store '()))

;; 1.3 错误信息包含模型与 provider，便于诊断
(define (capture-route-error thunk)
  (catch 'pi-config-error thunk
    (lambda (err-type . info)
      (cond ((and (pair? info) (string? (car info))) (car info))
            ((and (pair? info) (pair? (car info)) (string? (caar info))) (caar info))
            (else (object->string (cons err-type info)))))))

(let ((msg (capture-route-error
             (lambda () (pi-route-provider-and-model "google" "k3" default-settings fixture-store '())))))
  (check-true (string-contains? msg "k3"))
  (check-true (string-contains? msg "google"))
  (check-true (string-contains? msg "--list-models")))

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

;; 2.3.2 MiniMax-M3.1-Flash 自动路由至 minimax
(let-values (((p m) (pi-route-provider-and-model #f "MiniMax-M3.1-Flash" default-settings fixture-store '())))
  (check p => "minimax")
  (check m => "MiniMax-M3.1-Flash"))

;; 2.3.1 glm-5.3 / glm-5.3-flash 自动路由至 glm（models.json 合并目录，任务 028）
(let-values (((p m) (pi-route-provider-and-model #f "glm-5.3" default-settings fixture-merged '())))
  (check p => "glm")
  (check m => "glm-5.3"))

(let-values (((p m) (pi-route-provider-and-model #f "glm-5.3-flash" default-settings fixture-merged '())))
  (check p => "glm")
  (check m => "glm-5.3-flash"))

;; 2.4 未指定 provider 且模型未命中任何 provider 时报 pi-config-error（不再兜底）
(check-catch 'pi-config-error
  (pi-route-provider-and-model #f "custom-unknown-model" default-settings fixture-store '()))

(let ((msg (capture-route-error
             (lambda () (pi-route-provider-and-model #f "custom-unknown-model" default-settings fixture-store '())))))
  (check-true (string-contains? msg "custom-unknown-model"))
  (check-true (string-contains? msg "--list-models")))

;; 2.5 未指定 provider 且模型命中内置目录（未写入 models-store）时正常路由
(let-values (((p m) (pi-route-provider-and-model #f "gemini-2.5-flash" default-settings fixture-store '())))
  (check p => "google")
  (check m => "gemini-2.5-flash"))

;; 3. 未显式指定 provider 且未显式指定 model 时，取 settings 或系统默认
(let-values (((p m) (pi-route-provider-and-model #f #f default-settings fixture-store '())))
  (check p => "google")
  (check m => "gemini-3.8-flash"))

;; 4. 环境变量 GF_PI_MODEL 显式模型生效并触发跨 provider 路由
(let-values (((p m) (pi-route-provider-and-model #f #f default-settings fixture-store '(("GF_PI_MODEL" . "k3")))))
  (check p => "kimi-coding")
  (check m => "k3"))

;; 5. 环境变量 GF_PI_MODEL 未登记模型同样报错
(check-catch 'pi-config-error
  (pi-route-provider-and-model #f #f default-settings fixture-store '(("GF_PI_MODEL" . "env-unknown-model"))))

(check-report)
