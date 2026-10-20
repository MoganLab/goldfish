(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (liii check)
        (liii string)
        (liii pi)
        (liii pi args)
        (liii pi config))

(check-set-mode! 'report-failed)

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/default")
      "tests/fixtures/default"
      "tools/pi/tests/fixtures/default"))
(define INVALID_CMD_DIR
  (if (file-exists? "tests/fixtures/invalid_cmd")
      "tests/fixtures/invalid_cmd"
      "tools/pi/tests/fixtures/invalid_cmd"))
;; 等价于「无用户配置」的自建 fixture，替代依赖 cwd 的相对路径占位目录
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
    (check (cdr (assoc 'context-window meta)) => 2097152)))

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

;; 5. CLI --provider "anthropic" 配合 $TEST_ANTHROPIC_KEY 插值与 ANTHROPIC_AUTH_TOKEN
(let* ((args (parse-pi-args '("--provider" "anthropic")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("TEST_ANTHROPIC_KEY" . "sk-ant-api-987654321")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "anthropic")
  (check (cdr (assoc 'model-id cfg)) => "claude-opus-4-8")
  (check (cdr (assoc 'api-key cfg)) => "sk-ant-api-987654321")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'base-url meta)) => "https://api.anthropic.com")
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 128000)))

(let* ((args (parse-pi-args '("--provider" "anthropic")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)
              ("ANTHROPIC_AUTH_TOKEN" . "auth-token-bearer-123")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "anthropic")
  (check (cdr (assoc 'api-key cfg)) => "auth-token-bearer-123"))

;; 6. 未知模型：直接报 pi-config-error（不再 warning + 兜底）
(let ((args (parse-pi-args '("--model" "custom-experimental-model")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR))))
  (check-catch 'pi-config-error
    (resolve-pi-config args env)))

;; 7. auth.json 含有 !command 格式的密钥时抛出 pi-config-error
(let ((args (parse-pi-args '()))
      (env `(("PI_CODING_AGENT_DIR" . ,INVALID_CMD_DIR))))
  (check-catch 'pi-config-error
    (resolve-pi-config args env)))

;; 8. 工具增量配置测试（settings 增删工具）
(let ((settings-with-delta '((default-tools . ("+grep" "-write"))))
      (args-empty (parse-pi-args '()))
      (args-override (parse-pi-args '("--tools" "read,bash"))))
  (check (resolve-active-tools args-empty settings-with-delta)
    => '("read" "bash" "edit" "grep"))
  (check (resolve-active-tools args-override settings-with-delta)
    => '("read" "bash")))

;; 8.1 --exclude-tools 排除测试与排除优先
(let ((args-ex (parse-pi-args '("--exclude-tools" "bash,write,edit")))
      (args-both (parse-pi-args '("--tools" "read,bash,edit" "--exclude-tools" "bash")))
      (args-unknown (parse-pi-args '("--exclude-tools" "foo,bash")))
      (args-spaces (parse-pi-args '("--exclude-tools" " bash , edit ")))
      (settings '((default-tools . ("read" "bash" "edit" "write")))))
  (check (resolve-active-tools args-ex settings) => '("read"))
  (check (resolve-active-tools args-both settings) => '("read" "edit"))
  (check (resolve-active-tools args-unknown settings) => '("read" "edit" "write"))
  (check (resolve-active-tools args-spaces settings) => '("read" "write")))

;; 8.2 resolve-pi-config 端到端 tools 解析验证
(let* ((args (parse-pi-args '("--exclude-tools" "bash,write,edit")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'tools cfg)) => '("read")))

;; 8.3 --no-tools 与 --no-builtin-tools 支持与关闭优先
(let ((args-no-tools (parse-pi-args '("--no-tools")))
      (args-no-builtin (parse-pi-args '("--no-builtin-tools")))
      (args-override-tools (parse-pi-args '("--tools" "read,bash" "--no-tools")))
      (args-override-builtin (parse-pi-args '("--tools" "read,bash" "--no-builtin-tools")))
      (args-override-exclude (parse-pi-args '("--exclude-tools" "bash" "--no-tools")))
      (args-override-all (parse-pi-args '("--tools" "read,bash" "--exclude-tools" "bash" "--no-tools")))
      (settings '((default-tools . ("read" "bash" "edit" "write")))))
  (check (resolve-active-tools args-no-tools settings) => '())
  (check (resolve-active-tools args-no-builtin settings) => '())
  (check (resolve-active-tools args-override-tools settings) => '())
  (check (resolve-active-tools args-override-builtin settings) => '())
  (check (resolve-active-tools args-override-exclude settings) => '())
  (check (resolve-active-tools args-override-all settings) => '()))

;; 8.4 resolve-pi-config 端到端 no-tools 验证
(let* ((args (parse-pi-args '("--no-tools")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'tools cfg)) => '()))

(let* ((args (parse-pi-args '("--no-builtin-tools")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'tools cfg)) => '()))

;; 9. CLI --provider "glm" 默认模型、端点与凭据解析 (glm 由 models.json 提供)
(let* ((args (parse-pi-args '("--provider" "glm")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("ANTHROPIC_AUTH_TOKEN" . "glm-token-xyz-123")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "glm")
  (check (cdr (assoc 'model-id cfg)) => "glm-5.3")
  (check (cdr (assoc 'api-key cfg)) => "glm-token-xyz-123")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'base-url meta)) => "https://open.bigmodel.cn/api/anthropic")
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 131072)))

;; 9.1 GLM_MODEL 环境变量指定已登记模型（models.json 合并目录）
(let* ((args (parse-pi-args '("--provider" "glm")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
              ("GLM_MODEL" . "glm-5.3")
              ("GLM_API_KEY" . "glm-key-456")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "glm")
  (check (cdr (assoc 'model-id cfg)) => "glm-5.3")
  (check (cdr (assoc 'api-key cfg)) => "glm-key-456")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 1000000)))

;; 9.2 GLM_MODEL 环境变量指向未登记模型时报 pi-config-error
(let ((args (parse-pi-args '("--provider" "glm")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)
             ("GLM_MODEL" . "glm-4-air")
             ("GLM_API_KEY" . "glm-key-456"))))
  (check-catch 'pi-config-error
    (resolve-pi-config args env)))

(let* ((args (parse-pi-args '("--provider" "glm")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR))))
  (let ((cfg (resolve-pi-config args env)))
    (check (cdr (assoc 'provider cfg)) => "glm")
    (check (cdr (assoc 'api-key cfg)) => #f)))

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
    ;; 生产语义：deepseek 端点一律归一化为 anthropic 兼容端点
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
;; 12.1 --model k3 未显式指定 provider 时，自动路由到 kimi-coding
(let* ((args (parse-pi-args '("--model" "k3")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "kimi-coding")
  (check (cdr (assoc 'model-id cfg)) => "k3")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'base-url meta)) => "https://api.kimi.com/coding/v1")
    (check (cdr (assoc 'context-window meta)) => 1048576)))

;; 12.2 --model glm-5.3 未显式指定 provider 时，自动路由到 glm
(let* ((args (parse-pi-args '("--model" "glm-5.3")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "glm")
  (check (cdr (assoc 'model-id cfg)) => "glm-5.3"))

;; 12.3 --model deepseek-v4-flash 未显式指定 provider 时，自动路由到 deepseek
(let* ((args (parse-pi-args '("--model" "deepseek-v4-flash")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'model-id cfg)) => "deepseek-v4-flash"))

;; 12.4 显式 --provider 与 --model 归属不匹配时直接报错（不允许 fallback）
(let ((args (parse-pi-args '("--provider" "google" "--model" "k3")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR))))
  (check-catch 'pi-config-error
    (resolve-pi-config args env)))

;; 12.5 未指定 provider 且模型未命中全部 provider 时报错（不再兜底）
(let ((args (parse-pi-args '("--model" "non-existent-anywhere-xyz")))
      (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR))))
  (check-catch 'pi-config-error
    (resolve-pi-config args env)))

;; 13. thinking-level 配置与覆盖测试
;; 13.1 默认从 settings.defaultThinkingLevel 读取（fixture 中为 medium）
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'thinking-level cfg)) => "medium"))

;; 13.2 CLI --thinking-level low 覆盖 settings
(let* ((args (parse-pi-args '("--thinking-level" "low")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'thinking-level cfg)) => "low"))

;; 13.3 CLI --thinking-level off 覆盖 settings
(let* ((args (parse-pi-args '("--thinking-level" "off")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'thinking-level cfg)) => "off"))

;; 13.4 未配置 settings 且未传 CLI 参数时默认为 off
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)
              ("ANTHROPIC_AUTH_TOKEN" . "test-key")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'thinking-level cfg)) => "off"))

;; 14. 严格匹配下正常命中：内置目录模型（gemini-2.5-flash）不被误判
(let* ((args (parse-pi-args '("--provider" "google" "--model" "gemini-2.5-flash")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-2.5-flash")
  (check (cdr (assoc 'matched? (cdr (assoc 'model-meta cfg)))) => #t))

(check-report)
