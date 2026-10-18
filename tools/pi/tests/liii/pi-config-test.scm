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
  (check (cdr (assoc 'model-id cfg)) => "claude-3-7-sonnet-20250219")
  (check (cdr (assoc 'api-key cfg)) => "sk-ant-api-987654321")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'base-url meta)) => "https://api.anthropic.com")
    (check (cdr (assoc 'context-window meta)) => 200000)
    (check (cdr (assoc 'max-tokens meta)) => 8192)))

(let* ((args (parse-pi-args '("--provider" "anthropic")))
       (env `(("PI_CODING_AGENT_DIR" . "non_existent_dir_xyz")
              ("ANTHROPIC_AUTH_TOKEN" . "auth-token-bearer-123")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "anthropic")
  (check (cdr (assoc 'api-key cfg)) => "auth-token-bearer-123"))

;; 6. 未知自定义模型：使用兜底元数据，matched? 为 #f
(let* ((args (parse-pi-args '("--model" "custom-experimental-model")))
       (env `(("PI_CODING_AGENT_DIR" . ,FIXTURE_DIR)))
       (cfg (resolve-pi-config args env))
       (meta (cdr (assoc 'model-meta cfg))))
  (check (cdr (assoc 'matched? meta)) => #f)
  (check (cdr (assoc 'context-window meta)) => 128000)
  (check (cdr (assoc 'max-tokens meta)) => 16384))

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

(check-report)
