(import (scheme process-context)
        (liii check)
        (liii string)
        (liii os)
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

;; 哨兵 env 表：非空但不含任何目标键。
;; %safe-lookup-env 以 (pair? opt-env) 判定，空表会读真实进程环境。
(define NO-ENV '(("__PI_TEST_NO_ENV__" . "1")))

;; 真实进程环境污染项
(define POLLUTION
  '(("GF_PI_MODEL" . "polluted-model")
    ("ANTHROPIC_DEFAULT_SONNET_MODEL" . "polluted-sonnet-model")
    ("DEEPSEEK_MODEL" . "polluted-deepseek-model")
    ("GLM_MODEL" . "polluted-glm-model")
    ("MINIMAX_MODEL" . "polluted-minimax-model")))

(define (with-env-vars vars thunk)
  "把 vars（(名称 . 值) 列表）临时写入真实进程环境，执行 thunk 后完整还原。"
  (let ((saved (map (lambda (kv) (cons (car kv) (get-environment-variable (car kv))))
                    vars)))
    (for-each (lambda (kv) (putenv (car kv) (cdr kv))) vars)
    (let ((result (catch #t thunk (lambda args (apply throw args)))))
      (for-each (lambda (kv)
                  (if (cdr kv)
                    (putenv (car kv) (cdr kv))
                    (unsetenv (car kv))
                  ) ;if
                ) ;for-each
               saved)
      result
    ) ;let
  ) ;let
) ;define

;; 1. 显式指定 --provider 且模型属于该 provider 时正常命中
(let-values (((p m) (pi-route-provider-and-model "google" "gemini-3.8-flash" default-settings fixture-store NO-ENV)))
  (check p => "google")
  (check m => "gemini-3.8-flash"))

(let-values (((p m) (pi-route-provider-and-model "kimi-coding" "k3" default-settings fixture-store NO-ENV)))
  (check p => "kimi-coding")
  (check m => "k3"))

(let-values (((p m) (pi-route-provider-and-model "deepseek" "deepseek-v4-flash" default-settings fixture-merged NO-ENV)))
  (check p => "deepseek")
  (check m => "deepseek-v4-flash"))

(let-values (((p m) (pi-route-provider-and-model "minimax" "MiniMax-M3.1-Flash" default-settings fixture-merged NO-ENV)))
  (check p => "minimax")
  (check m => "MiniMax-M3.1-Flash"))

(let-values (((p m) (pi-route-provider-and-model "minimax-cn" "MiniMax-M2.7" default-settings fixture-store NO-ENV)))
  (check p => "minimax-cn")
  (check m => "MiniMax-M2.7"))

;; 1.1 显式 provider 与模型归属不匹配时报 pi-config-error（不允许 fallback 继续）
(check-catch 'pi-config-error
  (pi-route-provider-and-model "google" "k3" default-settings fixture-store NO-ENV))

(check-catch 'pi-config-error
  (pi-route-provider-and-model "kimi-coding" "deepseek-v4-flash" default-settings fixture-merged NO-ENV))

;; 1.2 显式 provider 下模型在 models-store 查无记录时报 pi-config-error
(check-catch 'pi-config-error
  (pi-route-provider-and-model "google" "non-existent-model-xyz" default-settings fixture-store NO-ENV))

;; 1.3 错误信息包含模型与 provider，便于诊断
(define (capture-route-error thunk)
  (catch 'pi-config-error thunk
    (lambda (err-type . info)
      (cond ((and (pair? info) (string? (car info))) (car info))
            ((and (pair? info) (pair? (car info)) (string? (caar info))) (caar info))
            (else (object->string (cons err-type info)))))))

(let ((msg (capture-route-error
             (lambda () (pi-route-provider-and-model "google" "k3" default-settings fixture-store NO-ENV)))))
  (check-true (string-contains? msg "k3"))
  (check-true (string-contains? msg "google"))
  (check-true (string-contains? msg "--list-models")))

;; 2. 未指定 --provider 时，按 --model 跨 provider 自动路由
;; 2.1 k3 自动路由至 kimi-coding
(let-values (((p m) (pi-route-provider-and-model #f "k3" default-settings fixture-store NO-ENV)))
  (check p => "kimi-coding")
  (check m => "k3"))

;; 2.2 glm-4.5-air 自动路由至 glm (来自 fixture-merged)
(let-values (((p m) (pi-route-provider-and-model #f "glm-4.5-air" default-settings fixture-merged NO-ENV)))
  (check p => "glm")
  (check m => "glm-4.5-air"))

;; 2.3 deepseek-v4-pro 自动路由至 deepseek
(let-values (((p m) (pi-route-provider-and-model #f "deepseek-v4-pro" default-settings fixture-store NO-ENV)))
  (check p => "deepseek")
  (check m => "deepseek-v4-pro"))

;; 2.3.2 MiniMax-M3.1-Flash 自动路由至 minimax
(let-values (((p m) (pi-route-provider-and-model #f "MiniMax-M3.1-Flash" default-settings fixture-merged NO-ENV)))
  (check p => "minimax")
  (check m => "MiniMax-M3.1-Flash"))

;; 2.3.1 glm-5.3 / glm-5.3-flash 自动路由至 glm（models.json 合并目录）
(let-values (((p m) (pi-route-provider-and-model #f "glm-5.3" default-settings fixture-merged NO-ENV)))
  (check p => "glm")
  (check m => "glm-5.3"))

;; 2.4 未指定 provider 且模型未命中任何 provider 时报 pi-config-error
(check-catch 'pi-config-error
  (pi-route-provider-and-model #f "custom-unknown-model" default-settings fixture-store NO-ENV))

;; 2.5 未指定 provider 且模型命中内置目录时正常路由
(let-values (((p m) (pi-route-provider-and-model #f "gemini-2.5-flash" default-settings fixture-store NO-ENV)))
  (check p => "google")
  (check m => "gemini-2.5-flash"))

;; 3. 未显式指定 provider 且未显式指定 model 时，取 settings 或系统默认
(let-values (((p m) (pi-route-provider-and-model #f #f default-settings fixture-store NO-ENV)))
  (check p => "google")
  (check m => "gemini-3.8-flash"))

;; 4. 环境变量 GF_PI_MODEL 显式模型生效并触发跨 provider 路由
(let-values (((p m) (pi-route-provider-and-model #f #f default-settings fixture-store '(("GF_PI_MODEL" . "k3")))))
  (check p => "kimi-coding")
  (check m => "k3"))

;; 5. 环境变量 GF_PI_MODEL 未登记模型同样报错
(check-catch 'pi-config-error
  (pi-route-provider-and-model #f #f default-settings fixture-store '(("GF_PI_MODEL" . "env-unknown-model"))))

;; 6. 默认模型兜底回退：当 provider 的默认模型不在目录中时，回退到该 provider 目录的第一个模型
(let* ((custom-store
        (list (cons "custom-prov"
                    (list (cons "models"
                                (list (list (cons "id" "fallback-first-model")
                                            (cons "name" "First Model"))))))))
       (prov-settings '(("default-provider" . "custom-prov") ("default-model" . "non-existent-default"))))
  (let-values (((p m) (pi-route-provider-and-model "custom-prov" #f prov-settings custom-store NO-ENV)))
    (check p => "custom-prov")
    (check m => "fallback-first-model")))

;; 7. 污染环境下路由结果不变（防回归：真实进程环境不得影响 provider/model 路由）
(let ((res (with-env-vars POLLUTION
             (lambda ()
               (call-with-values
                 (lambda ()
                   (pi-route-provider-and-model "deepseek" #f default-settings fixture-merged NO-ENV)
                 ) ;lambda
                 (lambda (p m) (list p m))
               ) ;call-with-values
             ) ;lambda
           ) ;with-env-vars
           )) ;
  (check (car res) => "deepseek")
  (check (cadr res) => "deepseek-v4-pro"))

;; 7.1 污染环境下默认 provider/model 仍取自 settings（不受 GF_PI_MODEL 等影响）
(let ((res (with-env-vars POLLUTION
             (lambda ()
               (call-with-values
                 (lambda ()
                   (pi-route-provider-and-model #f #f default-settings fixture-store NO-ENV)
                 ) ;lambda
                 (lambda (p m) (list p m))
               ) ;call-with-values
             ) ;lambda
           ) ;with-env-vars
           )) ;
  (check (car res) => "google")
  (check (cadr res) => "gemini-3.8-flash"))

;; 8. provider/model 斜杠语法路由解析
(let-values (((p m) (pi-route-provider-and-model #f "zai-coding-cn/glm-5.3-flash" default-settings fixture-store NO-ENV)))
  (check p => "zai-coding-cn")
  (check m => "glm-5.3-flash"))

(let-values (((p m) (pi-route-provider-and-model #f "anthropic/claude-opus-4-8" default-settings fixture-store NO-ENV)))
  (check p => "anthropic")
  (check m => "claude-opus-4-8"))

(check-report)
