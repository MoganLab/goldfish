(import (scheme base)
  (scheme file)
  (scheme read)
  (liii check)
  (liii subprocess)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

;; [020] deepseek provider 支持 验收脚本

(set! *load-path* (cons "tools/pi" *load-path*))
(eval '(import (liii pi)) (rootlet))
(eval '(import (liii pi deepseek)) (rootlet))
(eval '(import (liii pi auth)) (rootlet))
(eval '(import (liii pi models)) (rootlet))
(eval '(import (liii pi config)) (rootlet))
(eval '(import (liii pi args)) (rootlet))
(eval '(import (liii pi config record)) (rootlet))

;; ============================================================================
;; 验收点 1: (liii pi deepseek) 子库存在且可 import
;; ============================================================================

(check-true (file-exists? "tools/pi/liii/pi/deepseek.scm"))
(check-true (procedure? deepseek-map-stop-reason))
(check-true (procedure? deepseek-map-usage))
(check-true (procedure? deepseek-build-tool-declarations))
(check-true (procedure? deepseek-build-messages))
(check-true (procedure? deepseek-build-request-body))
(check-true (procedure? deepseek-parse-response))
(check-true (procedure? deepseek-is-retriable-error?))
(check-true (procedure? deepseek-build-tool-declarations-from-names))
(check-true (procedure? deepseek-request))
(check-true (procedure? deepseek-request-stream))

;; ============================================================================
;; 验收点 2: deepseek 凭据与模型元数据解析正常
;; ============================================================================

;; 2.1 auth-alist 存储条目优先
(let ((auth '(("deepseek" . "stored-ds-key-020")))
      (env '(("DEEPSEEK_API_KEY" . "env-token-xyz")))
     ) ;
  (check (pi-resolve-provider-key "deepseek" auth env) => "stored-ds-key-020")
) ;let

;; 2.2 环境变量 DEEPSEEK_API_KEY 兜底
(let ((auth '()) (env '(("DEEPSEEK_API_KEY" . "env-ds-key-020"))))
  (check (pi-resolve-provider-key "deepseek" auth env) => "env-ds-key-020")
) ;let

;; 2.3 环境变量 ANTHROPIC_AUTH_TOKEN 兜底
(let ((auth '()) (env '(("ANTHROPIC_AUTH_TOKEN" . "env-token-xyz"))))
  (check (pi-resolve-provider-key "deepseek" auth env) => "env-token-xyz")
) ;let

;; 2.4 默认元数据：端点、上下文与 max-tokens
(let ((meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" '() '())))
  (check-true (pi-model-metadata? meta))
  (check (pi-model-metadata-id meta) => "deepseek-chat")
  (check (pi-model-metadata-base-url meta)
    =>
    "https://api.deepseek.com/anthropic"
  ) ;check
  (check (pi-model-metadata-context-window meta) => 128000)
  (check (pi-model-metadata-max-tokens meta) => 8192)
) ;let

;; 2.5 DEEPSEEK_BASE_URL 覆盖端点
(let* ((env '(("DEEPSEEK_BASE_URL" . "https://custom.deepseek.com/anthropic")))
       (meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" '() env))
      ) ;
  (check (pi-model-metadata-base-url meta)
    =>
    "https://custom.deepseek.com/anthropic"
  ) ;check
) ;let*

;; 2.6 resolve-pi-config 默认模型为 deepseek-chat
(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env '(("PI_CODING_AGENT_DIR" . "non_existent_dir_020")
              ("DEEPSEEK_API_KEY" . "test-key-020"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'model-id cfg)) => "deepseek-chat")
  (check (cdr (assoc 'api-key cfg)) => "test-key-020")
) ;let*

;; 2.7 DEEPSEEK_MODEL 覆盖默认模型
(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env '(("PI_CODING_AGENT_DIR" . "non_existent_dir_020")
              ("DEEPSEEK_MODEL" . "deepseek-reasoner")
              ("DEEPSEEK_API_KEY" . "test-key-020"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'model-id cfg)) => "deepseek-reasoner")
) ;let*

;; ============================================================================
;; 验收点 3: deepseek 测试文件齐全且过
;; ============================================================================

(check-true (file-exists? "tools/pi/tests/liii/pi/deepseek/deepseek-map-stop-reason-test.scm"
            ) ;file-exists?
) ;check-true
(check-true (file-exists? "tools/pi/tests/liii/pi/deepseek/deepseek-map-usage-test.scm")
) ;check-true
(check-true (file-exists? "tools/pi/tests/liii/pi/deepseek/deepseek-build-request-body-test.scm"
            ) ;file-exists?
) ;check-true
(check-true (file-exists? "tools/pi/tests/liii/pi/deepseek/deepseek-parse-response-test.scm"
            ) ;file-exists?
) ;check-true
(check-true (file-exists? "tools/pi/tests/liii/pi/deepseek/deepseek-is-retriable-error-test.scm"
            ) ;file-exists?
) ;check-true

(check (run "bin/gf test tools/pi/tests/liii/pi/deepseek/deepseek-map-stop-reason-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/deepseek/deepseek-map-usage-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/deepseek/deepseek-build-request-body-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/deepseek/deepseek-parse-response-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/deepseek/deepseek-is-retriable-error-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check

;; ============================================================================
;; 验收点 4: bin/gf test 全量过
;; ============================================================================

(check (run "bin/gf test tools/pi/tests/liii/" :timeout 120) => 0)

;; ============================================================================
;; 验收点 5: bin/gf pi --help 无异常
;; ============================================================================

(check (run "bin/gf pi --help" :timeout 30) => 0)

;; ============================================================================
;; 验收点 6: 凭据缺失路径退出码 1
;; ============================================================================

(check (run "env -u ANTHROPIC_AUTH_TOKEN -u DEEPSEEK_API_KEY -u ANTHROPIC_API_KEY HOME=/tmp/non_existent_020 PI_CODING_AGENT_DIR=/tmp/non_existent_020 bin/gf pi --provider deepseek -p 'test'"
         :timeout 30
       ) ;run
  =>
  1
) ;check

;; ============================================================================
;; 验收点 7: 门禁检查通过
;; ============================================================================

(check (run "timeout 600 xmake b goldfish" :timeout 650) => 0)
(check (run "bin/gf fmt" :timeout 120) => 0)
(check (run "bin/gf check" :timeout 120) => 0)

(check-report)
