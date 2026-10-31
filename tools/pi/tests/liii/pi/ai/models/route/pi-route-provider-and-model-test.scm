(import (scheme process-context)
  (liii check)
  (liii string)
  (liii os)
  (liii pi ai models)
) ;import

(check-set-mode! 'report-failed)

;; pi-route-provider-and-model
;; 统一解析 CLI 参数、环境变量与配置，确定生效的 provider 与 model-id

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"
  ) ;if
) ;define

(define fixture-store (pi-load-models-store FIXTURE_DIR))

(define fixture-merged
  (pi-merge-models-json-into-store fixture-store
    (pi-load-models-json FIXTURE_DIR)
  ) ;pi-merge-models-json-into-store
) ;define

(define default-settings
  '(("default-provider" . "google") ("default-model" . "gemini-3.8-flash"))
) ;define

(define NO-ENV '(("__PI_TEST_NO_ENV__" . "1")))

;; 默认目录 = 内置目录 + fixture（可用性以目录为准，内置模型需并入目录）
(define default-catalog (pi-builtin-augment-all fixture-merged))

(define (check-route exp-p exp-m prov model . opt)
  (let ((store (if (pair? opt) (car opt) default-catalog))
        (env
          (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) NO-ENV)
        ) ;env
       ) ;
    (let-values (((p m) (pi-route-provider-and-model prov model default-settings store env)))
      (check p => exp-p)
      (check m => exp-m)
    ) ;let-values
  ) ;let
) ;define

;; 真实进程环境污染项

(define POLLUTION
  '(("GF_PI_MODEL" . "polluted-model")
    ("ANTHROPIC_DEFAULT_SONNET_MODEL" . "polluted-sonnet-model")
    ("DEEPSEEK_MODEL" . "polluted-deepseek-model")
    ("GLM_MODEL" . "polluted-glm-model"))
) ;define

(define (with-env-vars vars thunk)
  "把 vars 临时写入真实进程环境，执行 thunk 后完整还原。"
  (let ((saved
          (map
            (lambda (kv) (cons (car kv) (get-environment-variable (car kv))))
            vars
          ) ;map
        ) ;saved
       ) ;
    (for-each (lambda (kv) (putenv (car kv) (cdr kv))) vars)
    (let ((result (catch #t thunk (lambda args (apply throw args)))))
      (for-each
        (lambda (kv) (if (cdr kv) (putenv (car kv) (cdr kv)) (unsetenv (car kv))))
        saved
      ) ;for-each
      result
    ) ;let
  ) ;let
) ;define

;; 1. 显式指定 --provider 且模型属于该 provider 时正常命中
(check-route "google" "gemini-3.8-flash" "google" "gemini-3.8-flash")
(check-route "kimi-coding" "k3" "kimi-coding" "k3")
(check-route "deepseek" "deepseek-v4-flash" "deepseek" "deepseek-v4-flash"
  fixture-merged
) ;check-route
(check-route "minimax-cn" "MiniMax-M3.1-Flash" "minimax-cn" "MiniMax-M3.1-Flash"
  fixture-merged
) ;check-route
(check-route "minimax-cn" "MiniMax-M2.7" "minimax-cn" "MiniMax-M2.7")

;; 1.1 显式 provider 与模型归属不匹配时报 pi-config-error
(check-catch 'pi-config-error
  (pi-route-provider-and-model "google" "k3" default-settings fixture-store
    NO-ENV
  ) ;pi-route-provider-and-model
) ;check-catch
(check-catch 'pi-config-error
  (pi-route-provider-and-model "kimi-coding" "deepseek-v4-flash"
    default-settings fixture-merged NO-ENV
  ) ;pi-route-provider-and-model
) ;check-catch

;; 1.2 显式 provider 下模型在 models-store 查无记录时报 pi-config-error
(check-catch 'pi-config-error
  (pi-route-provider-and-model "google" "non-existent-model-xyz"
    default-settings fixture-store NO-ENV
  ) ;pi-route-provider-and-model
) ;check-catch

;; 1.3 错误信息包含模型与 provider，便于诊断

(define (capture-route-error thunk)
  (catch 'pi-config-error
    thunk
    (lambda (err-type . info)
      (cond
       ((and (pair? info) (string? (car info))) (car info))
       ((and (pair? info) (pair? (car info)) (string? (caar info))) (caar info))
       (else (object->string (cons err-type info)))
      ) ;cond
    ) ;lambda
  ) ;catch
) ;define

(let ((msg (capture-route-error (lambda ()
                                  (pi-route-provider-and-model "google" "k3"
                                    default-settings fixture-store NO-ENV
                                  ) ;pi-route-provider-and-model
                                ) ;lambda
           ) ;capture-route-error
      ) ;msg
     ) ;
  (check-true (string-contains? msg "k3"))
  (check-true (string-contains? msg "google"))
  (check-true (string-contains? msg "--list-models"))
) ;let

;; 2. 未指定 --provider 时，按 --model 跨 provider 自动路由
(check-route "kimi-coding" "k3" #f "k3")
(check-route "zai-coding-cn" "glm-4.5-air" #f "glm-4.5-air" default-catalog)
(check-route "deepseek" "deepseek-v4-pro" #f "deepseek-v4-pro")
(check-route "minimax-cn" "MiniMax-M3.1-Flash" #f "MiniMax-M3.1-Flash"
  default-catalog
) ;check-route
(check-route "zai-coding-cn" "glm-5.3" #f "glm-5.3" default-catalog)
(check-route "google" "gemini-2.5-flash" #f "gemini-2.5-flash")

;; 2.4 未指定 provider 且模型未命中任何 provider 时报 pi-config-error
(check-catch 'pi-config-error
  (pi-route-provider-and-model #f "custom-unknown-model" default-settings
    fixture-store NO-ENV
  ) ;pi-route-provider-and-model
) ;check-catch

;; 3. 未显式指定 provider 且未显式指定 model 时，取 settings 或系统默认
(check-route "google" "gemini-3.8-flash" #f #f)

;; 4. 环境变量 GF_PI_MODEL 显式模型生效并触发跨 provider 路由
(check-route "kimi-coding" "k3" #f #f default-catalog '(("GF_PI_MODEL" . "k3")))

;; 5. 环境变量 GF_PI_MODEL 未登记模型同样报错
(check-catch 'pi-config-error
  (pi-route-provider-and-model #f
    #f
    default-settings
    fixture-store
    '(("GF_PI_MODEL" . "env-unknown-model"))
  ) ;pi-route-provider-and-model
) ;check-catch

;; 6. 默认模型兜底回退：当 provider 的默认模型不在目录中时，回退到该 provider 目录的第一个模型
(let* ((custom-store
         (list
           (cons "custom-prov"
             (list
               (cons "models"
                 (list (list (cons "id" "fallback-first-model") (cons "name" "First Model")))
               ) ;cons
             ) ;list
           ) ;cons
         ) ;list
       ) ;custom-store
       (prov-settings '(("default-provider" . "custom-prov")
                        ("default-model" . "non-existent-default"))
       ) ;prov-settings
      ) ;
  (let-values (((p m)
                (pi-route-provider-and-model "custom-prov" #f prov-settings custom-store NO-ENV)
               ) ;
              ) ;
    (check p => "custom-prov")
    (check m => "fallback-first-model")
  ) ;let-values
) ;let*

;; 7. 污染环境下路由结果不变（防回归：真实进程环境不得影响 provider/model 路由）
(let ((res
        (with-env-vars POLLUTION
          (lambda ()
            (call-with-values (lambda ()
                                (pi-route-provider-and-model "deepseek" #f
                                  default-settings fixture-merged NO-ENV
                                ) ;pi-route-provider-and-model
                              ) ;lambda
              (lambda (p m) (list p m))
            ) ;call-with-values
          ) ;lambda
        ) ;with-env-vars
      ) ;res
     ) ;
  (check (car res) => "deepseek")
  (check (cadr res) => "deepseek-v4-pro")
) ;let

;; 7.1 污染环境下默认 provider/model 仍取自 settings
(let ((res
        (with-env-vars POLLUTION
          (lambda ()
            (call-with-values (lambda ()
                                (pi-route-provider-and-model #f #f default-settings fixture-store NO-ENV)
                              ) ;lambda
              (lambda (p m) (list p m))
            ) ;call-with-values
          ) ;lambda
        ) ;with-env-vars
      ) ;res
     ) ;
  (check (car res) => "google")
  (check (cadr res) => "gemini-3.8-flash")
) ;let

;; 8. provider/model 斜杠语法路由解析
(check-route "zai-coding-cn" "glm-5.3-flash" #f "zai-coding-cn/glm-5.3-flash")
(check-route "minimax-cn" "MiniMax-M3.1-Flash" #f "minimax-cn/MiniMax-M3.1-Flash")

;; 9. 守卫断言：遍历所有已知 provider，验证每个 provider 的默认模型均真实存在于其目录中
;; 9.1 验证所有 PI_SUPPORTED_PROVIDERS 的默认模型均在其内置目录中（自洽闭环）
(for-each
  (lambda (prov)
    (check-true (pi-provider-has-model? prov (pi-provider-default-model prov) default-catalog)
    ) ;check-true
  ) ;lambda
  PI_SUPPORTED_PROVIDERS
) ;for-each

;; 9.2 内置目录本身即完整目录：逐 provider 校验默认模型
(for-each
  (lambda (entry)
    (let ((prov (car entry)))
      (check-true (pi-provider-has-model? prov (pi-provider-default-model prov) default-catalog)
      ) ;check-true
    ) ;let
  ) ;lambda
  PI_PROVIDER_BUILTIN_MODELS
) ;for-each

;; 9.3 验证 minimax-cn 在未指定 model 时正确路由到默认模型 MiniMax-M3.1-Flash（无外部 models.json 纯内置环境）
(check-route "minimax-cn" "MiniMax-M3.1-Flash" "minimax-cn" #f)

(check-report)
