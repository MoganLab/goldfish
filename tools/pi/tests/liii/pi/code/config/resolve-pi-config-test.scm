(import (liii check)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"))
(define INVALID_CMD_DIR
  (if (file-exists? "tests/fixtures/invalid_cmd")
    "tests/fixtures/invalid_cmd"
    "tools/pi/tests/fixtures/invalid_cmd"))
(define CONFIG_EMPTY_DIR
  (if (file-exists? "tests/fixtures/config-empty")
    "tests/fixtures/config-empty"
    "tools/pi/tests/fixtures/config-empty"))

;; 1. 默认配置解析（基于 default fixture）
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-3.8-flash")
  (check (cdr (assoc 'api-key cfg)) => "test-google-key-from-auth-json")
  (check (cdr (assoc 'proxy-url cfg)) => "http://127.0.0.1:7890")
  (check (cdr (assoc 'tools cfg)) => '("read" "bash" "edit" "write"))
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 1048576)
    (check (cdr (assoc 'max-tokens meta)) => 65536)))

;; 2. CLI --model 覆盖 settings 与环境变量
(let* ((args (parse-pi-args '("--model" "gemini-2.5-pro")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("GF_PI_MODEL" . "should-be-ignored-by-cli-flag")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'model-id cfg)) => "gemini-2.5-pro")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'matched? meta)) => #t)
    ;; models-store.json 不再参与合并，取内置目录值
    (check (cdr (assoc 'context-window meta)) => 1048576)))

;; 3. GF_PI_MODEL 环境变量覆盖 settings.defaultModel
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("GF_PI_MODEL" . "gemini-2.5-pro")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'model-id cfg)) => "gemini-2.5-pro"))

;; 4. CLI --proxy 覆盖 settings.httpProxy
(let* ((args (parse-pi-args '("--proxy" "socks5h://127.0.0.1:1080")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'proxy-url cfg)) => "socks5h://127.0.0.1:1080"))

;; 5. CLI --provider "zai-coding-cn" 默认模型、端点与凭据解析（anthropic provider 已移除）
(let* ((args (parse-pi-args '("--provider" "zai-coding-cn")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("ZAI_CODING_CN_API_KEY" . "zai-key-987654321")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "zai-coding-cn")
  (check (cdr (assoc 'model-id cfg)) => "glm-5.3")
  (check (cdr (assoc 'api-key cfg)) => "zai-key-987654321")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'base-url meta)) => "https://open.bigmodel.cn/api/coding/paas/v4")
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 131072)))

;; 5.1 无任何凭据时 api-key 为 #f
(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)
              ("HOME" . "/non-existent")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'api-key cfg)) => #f))

;; 6. 未知模型：直接报 pi-config-error
(let ((args (parse-pi-args '("--model" "custom-experimental-model")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR))))
  (check-catch 'pi-config-error (resolve-pi-config args env)))

;; 7. auth.json 含有 !command 格式的密钥时抛出 pi-config-error
(let ((args (parse-pi-args '()))
      (env `(("PI_CODING_AGENT_DIR" . ,INVALID_CMD_DIR))))
  (check-catch 'pi-config-error (resolve-pi-config args env)))

;; 8.2 resolve-pi-config 端到端：settings 显式 defaultTools 完整列表优先
(let* ((args (parse-pi-args '("--exclude-tools" "bash,write,edit")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'tools cfg)) => '("read")))

;; 8.2b 无 settings defaultTools 时扩展 default-active 并入默认集
(let* ((args (parse-pi-args '("--exclude-tools" "bash,write,edit")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'tools cfg)) => '("read" "web_search")))

;; 8.4 resolve-pi-config 端到端 no-tools 验证
(let* ((args (parse-pi-args '("--no-tools")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'tools cfg)) => '()))

(let* ((args (parse-pi-args '("--no-builtin-tools")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'tools cfg)) => '()))

;; 9. CLI --provider "zai-coding-cn" 凭据环境变量链
(let* ((args (parse-pi-args '("--provider" "zai-coding-cn")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("GLM_API_KEY" . "glm-key-456")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "zai-coding-cn")
  (check (cdr (assoc 'model-id cfg)) => "glm-5.3")
  (check (cdr (assoc 'api-key cfg)) => "glm-key-456")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 1000000)))

;; 9.2 未登记模型时报 pi-config-error
(let ((args (parse-pi-args '("--provider" "zai-coding-cn" "--model" "glm-4-air")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
             ("GLM_API_KEY" . "glm-key-456"))))
  (check-catch 'pi-config-error (resolve-pi-config args env)))

;; 10. CLI --provider "deepseek" 默认模型、端点与凭据解析
(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)
              ("DEEPSEEK_API_KEY" . "ds-key-xyz-789")
              ("HOME" . "/non-existent")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'model-id cfg)) => "deepseek-v4-pro")
  (check (cdr (assoc 'api-key cfg)) => "ds-key-xyz-789")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'base-url meta)) => "https://api.deepseek.com/anthropic")
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 384000)))

(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("DEEPSEEK_MODEL" . "deepseek-v4-flash")
              ("DEEPSEEK_API_KEY" . "ds-key-xyz-789")
              ("HOME" . "/non-existent")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'model-id cfg)) => "deepseek-v4-flash")
  (check (cdr (assoc 'api-key cfg)) => "ds-key-xyz-789")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 1000000)))

(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)
              ("HOME" . "/non-existent"))))
  (let ((cfg (resolve-pi-config args env)))
    (check (cdr (assoc 'provider cfg)) => "deepseek")
    (check (cdr (assoc 'api-key cfg)) => #f)))

;; 12. --model 跨 provider 自动路由测试
(let* ((args (parse-pi-args '("--model" "k3")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "kimi-coding")
  (check (cdr (assoc 'model-id cfg)) => "k3")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'base-url meta)) => "https://api.kimi.com/coding/v1")
    (check (cdr (assoc 'context-window meta)) => 1048576)))

(let* ((args (parse-pi-args '("--model" "glm-5.3")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "zai-coding-cn")
  (check (cdr (assoc 'model-id cfg)) => "glm-5.3"))

(let* ((args (parse-pi-args '("--model" "deepseek-v4-flash")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'model-id cfg)) => "deepseek-v4-flash"))

;; 显式 --provider 与 --model 归属不匹配时直接报错
(let ((args (parse-pi-args '("--provider" "google" "--model" "k3")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR))))
  (check-catch 'pi-config-error (resolve-pi-config args env)))

;; 未指定 provider 且模型未命中全部 provider 时报错
(let ((args (parse-pi-args '("--model" "non-existent-anywhere-xyz")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR))))
  (check-catch 'pi-config-error (resolve-pi-config args env)))

;; 13. thinking-level 配置与覆盖测试
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'thinking-level cfg)) => "medium"))

(let* ((args (parse-pi-args '("--thinking" "low")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'thinking-level cfg)) => "low"))

(let* ((args (parse-pi-args '("--thinking" "off")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'thinking-level cfg)) => "off"))

(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)
              ("ANTHROPIC_AUTH_TOKEN" . "test-key")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'thinking-level cfg)) => "off"))

;; 14. 严格匹配下正常命中：内置目录模型不被误判
(let* ((args (parse-pi-args '("--provider" "google" "--model" "gemini-2.5-flash")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-2.5-flash")
  (check (cdr (assoc 'matched? (cdr (assoc 'model-meta cfg)))) => #t))

(check-report)
