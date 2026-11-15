;; resolve-pi-config
;; 根据命令行参数、环境变量与配置文件解析完整的运行配置。
;;
;; 语法
;; ----
;; (resolve-pi-config parsed-args . opt-env)
;;
;; 参数
;; ----
;; parsed-args : alist
;; parse-pi-args 生成的命令行参数。
;; opt-env : alist
;; 可选的环境变量表。
;;
;; 返回值
;; ----
;; alist
;; 解析后的运行配置。

(import (liii check)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"
  ) ;if
) ;define

(define INVALID_CMD_DIR
  (if (file-exists? "tests/fixtures/invalid_cmd")
    "tests/fixtures/invalid_cmd"
    "tools/pi/tests/fixtures/invalid_cmd"
  ) ;if
) ;define

(define CONFIG_EMPTY_DIR
  (if (file-exists? "tests/fixtures/config-empty")
    "tests/fixtures/config-empty"
    "tools/pi/tests/fixtures/config-empty"
  ) ;if
) ;define

;; per-model 思考级别覆盖 fixture（settings.modelThinkingLevels）

(define MODEL_THINKING_DIR
  (if (file-exists? "tests/fixtures/model-thinking")
    "tests/fixtures/model-thinking"
    "tools/pi/tests/fixtures/model-thinking"
  ) ;if
) ;define

;; 1. 默认配置解析（基于 default fixture）
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "google")
  (check (pi-cfg-ref cfg 'model-id) => "gemini-3.8-flash")
  (check (pi-cfg-ref cfg 'api-key) => "test-google-key-from-auth-json")
  (check (pi-cfg-ref cfg 'proxy-url) => "http://127.0.0.1:7890")
  (check (pi-cfg-ref cfg 'tools) => '("read" "bash" "edit" "write"))
  (let ((meta (pi-cfg-ref cfg 'model-meta)))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 1048576)
    (check (cdr (assoc 'max-tokens meta)) => 65536)
  ) ;let
) ;let*

;; 2. CLI --model 覆盖 settings 与环境变量
(let* ((args (parse-pi-args '("--model" "gemini-2.5-pro")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("GF_PI_MODEL" . "should-be-ignored-by-cli-flag"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'model-id) => "gemini-2.5-pro")
  (let ((meta (pi-cfg-ref cfg 'model-meta)))
    (check (cdr (assoc 'matched? meta)) => #t)
    ;; models-store.json 不再参与合并，取内置目录值
    (check (cdr (assoc 'context-window meta)) => 1048576)
  ) ;let
) ;let*

;; 3. GF_PI_MODEL 环境变量覆盖 settings.defaultModel
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("GF_PI_MODEL" . "gemini-2.5-pro"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'model-id) => "gemini-2.5-pro")
) ;let*

;; 4. CLI --proxy 覆盖 settings.httpProxy
(let* ((args (parse-pi-args '("--proxy" "socks5h://127.0.0.1:1080")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'proxy-url) => "socks5h://127.0.0.1:1080")
) ;let*

;; 5. CLI --provider "zai-coding-cn" 默认模型、端点与凭据解析（anthropic provider 已移除）
(let* ((args (parse-pi-args '("--provider" "zai-coding-cn")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("ZAI_CODING_CN_API_KEY" . "zai-key-987654321"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "zai-coding-cn")
  (check (pi-cfg-ref cfg 'model-id) => "glm-5.3")
  (check (pi-cfg-ref cfg 'api-key) => "zai-key-987654321")
  (let ((meta (pi-cfg-ref cfg 'model-meta)))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'base-url meta))
      =>
      "https://open.bigmodel.cn/api/coding/paas/v4"
    ) ;check
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 131072)
  ) ;let
) ;let*

;; 5.1 无任何凭据时 api-key 为 #f
(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)
              ("HOME" . "/non-existent")))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "deepseek")
  (check (pi-cfg-ref cfg 'api-key) => #f)
) ;let*

;; 6. 未知模型：直接报 pi-config-error
(let ((args (parse-pi-args '("--model" "custom-experimental-model")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
     ) ;
  (check-catch 'pi-config-error (resolve-pi-config args env))
) ;let

;; 7. auth.json 含有 !command 格式的密钥时抛出 pi-config-error
(let ((args (parse-pi-args '())) (env `(("PI_CODING_AGENT_DIR"
                                         . ,INVALID_CMD_DIR))))
  (check-catch 'pi-config-error (resolve-pi-config args env))
) ;let

;; 8.2 resolve-pi-config 端到端：settings 显式 defaultTools 完整列表优先
(let* ((args (parse-pi-args '("--exclude-tools" "bash,write,edit")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'tools) => '("read"))
) ;let*

;; 8.2b 无 settings defaultTools 时扩展 default-active 并入默认集
(let* ((args (parse-pi-args '("--exclude-tools" "bash,write,edit")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'tools) => '("read" "web_search"))
) ;let*

;; 8.4 resolve-pi-config 端到端 no-tools 验证
(let* ((args (parse-pi-args '("--no-tools")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'tools) => '())
) ;let*

(let* ((args (parse-pi-args '("--no-builtin-tools")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'tools) => '())
) ;let*

;; 9. CLI --provider "zai-coding-cn" 凭据环境变量链
(let* ((args (parse-pi-args '("--provider" "zai-coding-cn")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("GLM_API_KEY" . "glm-key-456")))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "zai-coding-cn")
  (check (pi-cfg-ref cfg 'model-id) => "glm-5.3")
  (check (pi-cfg-ref cfg 'api-key) => "glm-key-456")
  (let ((meta (pi-cfg-ref cfg 'model-meta)))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 1000000)
  ) ;let
) ;let*

;; 9.2 未登记模型时报 pi-config-error
(let ((args (parse-pi-args '("--provider" "zai-coding-cn" "--model" "glm-4-air")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
             ("GLM_API_KEY" . "glm-key-456")))
     ) ;
  (check-catch 'pi-config-error (resolve-pi-config args env))
) ;let

;; 10. CLI --provider "deepseek" 默认模型、端点与凭据解析
(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)
              ("DEEPSEEK_API_KEY" . "ds-key-xyz-789")
              ("HOME" . "/non-existent"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "deepseek")
  (check (pi-cfg-ref cfg 'model-id) => "deepseek-v4-pro")
  (check (pi-cfg-ref cfg 'api-key) => "ds-key-xyz-789")
  (let ((meta (pi-cfg-ref cfg 'model-meta)))
    (check (cdr (assoc 'base-url meta)) => "https://api.deepseek.com/anthropic")
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 384000)
  ) ;let
) ;let*

(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("DEEPSEEK_MODEL" . "deepseek-v4-flash")
              ("DEEPSEEK_API_KEY" . "ds-key-xyz-789")
              ("HOME" . "/non-existent"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "deepseek")
  (check (pi-cfg-ref cfg 'model-id) => "deepseek-v4-flash")
  (check (pi-cfg-ref cfg 'api-key) => "ds-key-xyz-789")
  (let ((meta (pi-cfg-ref cfg 'model-meta)))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 1000000)
  ) ;let
) ;let*

(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)
              ("HOME" . "/non-existent")))
      ) ;
  (let ((cfg (resolve-pi-config args env)))
    (check (pi-cfg-ref cfg 'provider) => "deepseek")
    (check (pi-cfg-ref cfg 'api-key) => #f)
  ) ;let
) ;let*

;; 12. --model 跨 provider 自动路由测试
(let* ((args (parse-pi-args '("--model" "k3")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "kimi-coding")
  (check (pi-cfg-ref cfg 'model-id) => "k3")
  (let ((meta (pi-cfg-ref cfg 'model-meta)))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'base-url meta)) => "https://api.kimi.com/coding/v1")
    (check (cdr (assoc 'context-window meta)) => 1048576)
  ) ;let
) ;let*

(let* ((args (parse-pi-args '("--model" "glm-5.3")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "zai-coding-cn")
  (check (pi-cfg-ref cfg 'model-id) => "glm-5.3")
) ;let*

(let* ((args (parse-pi-args '("--model" "deepseek-v4-flash")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "deepseek")
  (check (pi-cfg-ref cfg 'model-id) => "deepseek-v4-flash")
) ;let*

;; 显式 --provider 与 --model 归属不匹配时直接报错
(let ((args (parse-pi-args '("--provider" "google" "--model" "k3")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
     ) ;
  (check-catch 'pi-config-error (resolve-pi-config args env))
) ;let

;; 未指定 provider 且模型未命中全部 provider 时报错
(let ((args (parse-pi-args '("--model" "non-existent-anywhere-xyz")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
     ) ;
  (check-catch 'pi-config-error (resolve-pi-config args env))
) ;let

;; 13. thinking-level 配置与覆盖测试
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'thinking-level) => "medium")
) ;let*

(let* ((args (parse-pi-args '("--thinking" "low")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'thinking-level) => "low")
) ;let*

(let* ((args (parse-pi-args '("--thinking" "off")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'thinking-level) => "off")
) ;let*

;; 无 settings、无 CLI 时回退默认级别 medium（对齐上游 DEFAULT_THINKING_LEVEL）
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)
              ("ANTHROPIC_AUTH_TOKEN" . "test-key"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'thinking-level) => "medium")
) ;let*

;; 13.1 per-model 覆盖（settings.modelThinkingLevels）优先于全局默认，且被 CLI 压过
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,MODEL_THINKING_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'model-id) => "gemini-3.8-flash")
  (check (pi-cfg-ref cfg 'thinking-level) => "high")
) ;let*

(let* ((args (parse-pi-args '("--thinking" "low")))
       (env `(("PI_CODING_AGENT_DIR" . ,MODEL_THINKING_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'thinking-level) => "low")
) ;let*

;; 13.2 per-model 覆盖遇到不支持思考的模型时收敛为 off
(let* ((args (parse-pi-args '("--model" "plain-model")))
       (env `(("PI_CODING_AGENT_DIR" . ,MODEL_THINKING_DIR)
              ("GF_PI_MODEL" . "gemini-3.8-flash"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'model-id) => "plain-model")
  (check (pi-cfg-ref cfg 'supports-thinking) => #f)
  (check (pi-cfg-ref cfg 'thinking-level) => "off")
) ;let*

;; 14. 严格匹配下正常命中：内置目录模型不被误判
(let* ((args (parse-pi-args '("--provider" "google" "--model" "gemini-2.5-flash")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "google")
  (check (pi-cfg-ref cfg 'model-id) => "gemini-2.5-flash")
  (check
    (cdr (assoc 'matched? (pi-cfg-ref cfg 'model-meta)))
    =>
    #t
  ) ;check
) ;let*

(check-report)
