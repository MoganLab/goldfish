(import (scheme base)
  (scheme file)
  (scheme read)
  (liii check)
  (liii subprocess)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

;; [019] glm provider 支持 验收脚本

(set! *load-path* (cons "tools/pi" *load-path*))
(eval '(import (liii pi)) (rootlet))
(eval '(import (liii pi auth)) (rootlet))
(eval '(import (liii pi models)) (rootlet))
(eval '(import (liii pi config)) (rootlet))
(eval '(import (liii pi args)) (rootlet))
(eval '(import (liii pi config record)) (rootlet))

;; ============================================================================
;; 验收点 1: glm 在凭据解析中已注册并支持环境变量兜底
;; ============================================================================

;; 1.1 auth-alist 存储条目优先
(let ((auth '(("glm" . "stored-glm-key-019")))
      (env '(("ANTHROPIC_AUTH_TOKEN" . "env-token-xyz")))
     ) ;
  (check (pi-resolve-provider-key "glm" auth env) => "stored-glm-key-019")
) ;let

;; 1.2 环境变量 ANTHROPIC_AUTH_TOKEN 兜底
(let ((auth '()) (env '(("ANTHROPIC_AUTH_TOKEN" . "env-token-xyz"))))
  (check (pi-resolve-provider-key "glm" auth env) => "env-token-xyz")
) ;let

;; 1.3 环境变量 GLM_API_KEY 兜底
(let ((auth '()) (env '(("GLM_API_KEY" . "glm-key-123"))))
  (check (pi-resolve-provider-key "glm" auth env) => "glm-key-123")
) ;let

;; 1.4 环境变量 ANTHROPIC_API_KEY 兜底
(let ((auth '()) (env '(("ANTHROPIC_API_KEY" . "ant-key-456"))))
  (check (pi-resolve-provider-key "glm" auth env) => "ant-key-456")
) ;let

;; 1.5 无凭据时返回 #f
(let ((auth '()) (env '()))
  (check (pi-resolve-provider-key "glm" auth env) => #f)
) ;let

;; ============================================================================
;; 验收点 2: glm 在模型元数据与配置中已注册并支持端点/模型覆盖
;; ============================================================================

;; 2.1 默认元数据：端点、上下文与 max-tokens
(let ((meta (pi-resolve-model-metadata "glm" "glm-4-plus" '() '())))
  (check-true (pi-model-metadata? meta))
  (check (pi-model-metadata-id meta) => "glm-4-plus")
  (check (pi-model-metadata-base-url meta)
    =>
    "https://open.bigmodel.cn/api/anthropic"
  ) ;check
  (check (pi-model-metadata-context-window meta) => 128000)
  (check (pi-model-metadata-max-tokens meta) => 8192)
) ;let

;; 2.2 ANTHROPIC_BASE_URL 覆盖端点
(let* ((env '(("ANTHROPIC_BASE_URL"
               . "https://custom.bigmodel.cn/api/anthropic")))
       (meta (pi-resolve-model-metadata "glm" "glm-4-plus" '() env))
      ) ;
  (check (pi-model-metadata-base-url meta)
    =>
    "https://custom.bigmodel.cn/api/anthropic"
  ) ;check
) ;let*

;; 2.3 GF_PI_BASE_URL 覆盖端点
(let* ((env '(("GF_PI_BASE_URL" . "https://proxy.bigmodel.cn/api/anthropic")))
       (meta (pi-resolve-model-metadata "glm" "glm-4-plus" '() env))
      ) ;
  (check (pi-model-metadata-base-url meta)
    =>
    "https://proxy.bigmodel.cn/api/anthropic"
  ) ;check
) ;let*

;; 2.4 resolve-pi-config 默认模型为 glm-4-plus
(let* ((args (parse-pi-args '("--provider" "glm")))
       (env '(("PI_CODING_AGENT_DIR" . "non_existent_dir_019")
              ("GLM_API_KEY" . "test-key-019"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "glm")
  (check (cdr (assoc 'model-id cfg)) => "glm-4-plus")
  (check (cdr (assoc 'api-key cfg)) => "test-key-019")
) ;let*

;; 2.5 GLM_MODEL 覆盖默认模型
(let* ((args (parse-pi-args '("--provider" "glm")))
       (env '(("PI_CODING_AGENT_DIR" . "non_existent_dir_019")
              ("GLM_MODEL" . "glm-4-air")
              ("GLM_API_KEY" . "test-key-019"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'model-id cfg)) => "glm-4-air")
) ;let*

;; ============================================================================
;; 验收点 3: CLI 帮助信息与凭据缺失路径
;; ============================================================================

;; 3.1 bin/gf pi --help 无异常
(check (run "bin/gf pi --help" :timeout 30) => 0)

;; 3.2 凭据缺失时明确报错退出码 1
(check (run "env -u ANTHROPIC_AUTH_TOKEN -u GLM_API_KEY -u ANTHROPIC_API_KEY PI_CODING_AGENT_DIR=/tmp/non_existent_019 bin/gf pi --provider glm -p 'test'"
         :timeout 30
       ) ;run
  =>
  1
) ;check

;; ============================================================================
;; 验收点 4: 单测及全量测试
;; ============================================================================

;; 4.1 相关新增单测通过
(check (run "bin/gf test tools/pi/tests/liii/pi/pi-resolve-provider-key-test.scm"
         :timeout 60
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/pi-resolve-model-metadata-test.scm"
         :timeout 60
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-config-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-agent-test.scm" :timeout 60)
  =>
  0
) ;check

;; 4.2 tools/pi 全量单测通过
(check (run "bin/gf test tools/pi/tests/liii/" :timeout 120) => 0)

;; ============================================================================
;; 验收点 5: 门禁检查通过
;; ============================================================================

;; 5.1 构建
(check (run "timeout 600 xmake b goldfish" :timeout 650) => 0)

;; 5.2 格式化
(check (run "bin/gf fmt" :timeout 120) => 0)

;; 5.3 检查
(check (run "bin/gf check" :timeout 120) => 0)

(check-report)
