(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  ;; 显式导入 (scheme process-context)：get-environment-variable 未被 (liii string)
  ;; 等导出，若不显式导入会解析到 define-library 泄漏的破损绑定并段错误（goldfish 内核 bug）
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii pi)
  (liii pi args)
  (liii pi config)
  (liii pi models)
) ;import

(check-set-mode! 'report-failed)

;; [034] 验收脚本：单测 / 函数级严格校验 / 真实 CLI 退出码 1 / 门禁

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-034-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp-out " 2>&1") timeout)))
      (let ((out
              (if (and (file-exists? tmp-out) (path-file? (path tmp-out)))
                (path-read-text tmp-out)
                ""
              ) ;if
            ) ;out
           ) ;
        (when (file-exists? tmp-out)
          (delete-file tmp-out)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

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

(display "=== 验收点 1: TDD 单测（路由层/配置层/e2e） ===\n")
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-config-test.scm" 120)
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/models/route/pi-route-provider-and-model-test.scm"
         120
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-cli-e2e-test.scm" 120)
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-json-e2e-test.scm" 120)
  =>
  0
) ;check

(display "=== 验收点 2: 函数级严格校验（不匹配 / 未登记 / 正常命中） ===\n"
) ;display

(check-catch 'pi-config-error
  (pi-route-provider-and-model "google"
    "deepseek-v4-flash"
    default-settings
    fixture-merged
    '()
  ) ;pi-route-provider-and-model
) ;check-catch
(check-catch 'pi-config-error
  (pi-route-provider-and-model "kimi-coding"
    "deepseek-v4-flash"
    default-settings
    fixture-merged
    '()
  ) ;pi-route-provider-and-model
) ;check-catch

(let ((msg (capture-route-error (lambda ()
                                  (pi-route-provider-and-model "google"
                                    "deepseek-v4-flash"
                                    default-settings
                                    fixture-merged
                                    '()
                                  ) ;pi-route-provider-and-model
                                ) ;lambda
           ) ;capture-route-error
      ) ;msg
     ) ;
  (check-true (string-contains? msg "deepseek-v4-flash"))
  (check-true (string-contains? msg "google"))
  (check-true (string-contains? msg "--list-models"))
) ;let

(check-catch 'pi-config-error
  (pi-route-provider-and-model "google"
    "non-existent-model-xyz"
    default-settings
    fixture-store
    '()
  ) ;pi-route-provider-and-model
) ;check-catch

(let ((msg (capture-route-error (lambda ()
                                  (pi-route-provider-and-model #f
                                    "non-existent-model-xyz"
                                    default-settings
                                    fixture-store
                                    '()
                                  ) ;pi-route-provider-and-model
                                ) ;lambda
           ) ;capture-route-error
      ) ;msg
     ) ;
  (check-true (string-contains? msg "Unknown model"))
  (check-true (string-contains? msg "non-existent-model-xyz"))
) ;let

;; 2.5 正常匹配保持兼容：google / kimi-coding / deepseek / glm（models.json 合并目录）
(let-values (((p m)
              (pi-route-provider-and-model "google"
                "gemini-2.5-flash"
                default-settings
                fixture-store
                '()
              ) ;pi-route-provider-and-model
             ) ;
            ) ;
  (check p => "google")
  (check m => "gemini-2.5-flash")
) ;let-values
(let-values (((p m)
              (pi-route-provider-and-model "kimi-coding"
                "k3"
                default-settings
                fixture-store
                '()
              ) ;pi-route-provider-and-model
             ) ;
            ) ;
  (check p => "kimi-coding")
  (check m => "k3")
) ;let-values
(let-values (((p m)
              (pi-route-provider-and-model "deepseek"
                "deepseek-v4-flash"
                default-settings
                fixture-merged
                '()
              ) ;pi-route-provider-and-model
             ) ;
            ) ;
  (check p => "deepseek")
  (check m => "deepseek-v4-flash")
) ;let-values
(let-values (((p m)
              (pi-route-provider-and-model "glm"
                "glm-5.3"
                default-settings
                fixture-merged
                '()
              ) ;pi-route-provider-and-model
             ) ;
            ) ;
  (check p => "glm")
  (check m => "glm-5.3")
) ;let-values

;; 2.6 配置层正常命中：无 fallback 警告，模型元数据匹配
(let* ((args (parse-pi-args '("--provider" "google" "--model" "gemini-2.5-pro")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
       (meta (cdr (assoc 'model-meta cfg)))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-2.5-pro")
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'context-window meta)) => 2097152)
) ;let*

;; 2.7 配置层不匹配/未登记均直接抛 pi-config-error
(check-catch 'pi-config-error
  (resolve-pi-config (parse-pi-args '("--provider" "google" "--model"
                                      "deepseek-v4-flash"))
    `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR))
  ) ;resolve-pi-config
) ;check-catch
(check-catch 'pi-config-error
  (resolve-pi-config (parse-pi-args '("--provider" "google" "--model"
                                      "non-existent-model-xyz"))
    `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR))
  ) ;resolve-pi-config
) ;check-catch

(display "=== 验收点 3: 真实 CLI 行为（不匹配/未登记 → 退出码 1） ===\n"
) ;display

(let ((rc (%run-capture "mismatch"
            "timeout 60 bin/gf pi --provider google --model deepseek-v4-flash -p \"test\""
            90
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 1)
  (check-true (string-contains? (cdr rc) "deepseek-v4-flash"))
  (check-true (string-contains? (cdr rc) "google"))
  (check-true (string-contains? (cdr rc) "not available for provider"))
  (check-false (string-contains? (cdr rc) "Warning: Model"))
  (check-false (string-contains? (cdr rc) "using fallback defaults"))
) ;let

(let ((rc (%run-capture "unknown-explicit"
            "timeout 60 bin/gf pi --provider google --model non-existent-model-xyz -p \"test\""
            90
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 1)
  (check-true (string-contains? (cdr rc) "non-existent-model-xyz"))
  (check-false (string-contains? (cdr rc) "Warning: Model"))
  (check-false (string-contains? (cdr rc) "using fallback defaults"))
) ;let

(let ((rc (%run-capture "unknown-global"
            "timeout 60 bin/gf pi --model non-existent-model-xyz -p \"test\""
            90
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 1)
  (check-true (string-contains? (cdr rc) "Unknown model"))
  (check-true (string-contains? (cdr rc) "non-existent-model-xyz"))
  (check-false (string-contains? (cdr rc) "Warning: Model"))
) ;let

(let ((rc (%run-capture "list-models"
            "timeout 60 bin/gf pi --provider google --list-models"
            90
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "gemini-2.5-flash"))
) ;let

(display "=== 验收点 4: 门禁检查 ===\n")
(check (%run-shell "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-shell "timeout 120 bin/gf fmt" 120) => 0)
(check (%run-shell "timeout 120 bin/gf check" 120) => 0)
(check (%run-shell "timeout 60 bin/gf doc --build-json" 60) => 0)

(check-report)
