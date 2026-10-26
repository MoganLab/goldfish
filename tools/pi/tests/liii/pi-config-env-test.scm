;; pi-config-env 测试：配置解析的环境隔离
;;
;; 目标：resolve-pi-config 的解析结果只由「显式 env 表 + 仓库内 fixture」决定，
;; 不受开发者真实进程环境（PI_CODING_AGENT_DIR / ANTHROPIC_BASE_URL /
;; GF_PI_BASE_URL / DEEPSEEK_BASE_URL 等）污染，也不依赖本机 ~/.pi/agent/。

(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (scheme process-context)
        (liii check)
        (liii string)
        (liii os)
        (liii pi code cli args) (liii pi code config) (liii pi code core session-manager)
        (liii pi code cli args)
        (liii pi code config))

(check-set-mode! 'report-failed)

(define CONFIG_DIR
  (if (file-exists? "tests/fixtures/config")
      "tests/fixtures/config"
      "tools/pi/tests/fixtures/config"))
(define CONFIG_EMPTY_DIR
  (if (file-exists? "tests/fixtures/config-empty")
      "tests/fixtures/config-empty"
      "tools/pi/tests/fixtures/config-empty"))

;; 真实进程环境污染项：注入后解析结果必须保持不变
(define POLLUTION
  '(("PI_CODING_AGENT_DIR" . "/tmp/pi-polluted-agent-dir")
    ("ANTHROPIC_BASE_URL" . "https://polluted.example.com/anthropic")
    ("ANTHROPIC_AUTH_TOKEN" . "polluted-anthropic-token")
    ("ANTHROPIC_DEFAULT_SONNET_MODEL" . "polluted-sonnet-model")
    ("GF_PI_BASE_URL" . "https://polluted.example.com/gf-pi")
    ("GF_PI_CONTEXT_WINDOW" . "1")
    ("GF_PI_MAX_TOKENS" . "1")
    ("GF_PI_MODEL" . "polluted-model")
    ("DEEPSEEK_BASE_URL" . "https://polluted.example.com/deepseek")
    ("DEEPSEEK_MODEL" . "polluted-deepseek-model")
    ("DEEPSEEK_API_KEY" . "polluted-deepseek-key")
    ("GLM_MODEL" . "polluted-glm-model")
    ("GLM_API_KEY" . "polluted-glm-key")
    ("KIMI_API_KEY" . "polluted-openai-key")
    ("GEMINI_API_KEY" . "polluted-gemini-key")))

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

;; 1. deepseek 端点由自建 fixture 钉死（models-store.json 覆盖内置目录）
(let* ((args (parse-pi-args '("--provider" "deepseek")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_DIR)
              ("DEEPSEEK_API_KEY" . "ds-key-xyz-789")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'model-id cfg)) => "deepseek-v4-pro")
  (check (cdr (assoc 'api-key cfg)) => "ds-key-xyz-789")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    ;; fixture 中 baseUrl 为 https://api.deepseek.com，生产语义归一化为 /anthropic
    (check (cdr (assoc 'base-url meta)) => "https://api.deepseek.com/anthropic")
    (check (cdr (assoc 'name meta)) => "DeepSeek V4 Pro (fixture)")
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 384000)))

;; 2. glm 端点由自建 fixture 钉死（models.json 提供 glm provider）
(let* ((args (parse-pi-args '("--provider" "glm")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_DIR)
              ("ANTHROPIC_AUTH_TOKEN" . "glm-token-xyz-123")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "glm")
  (check (cdr (assoc 'model-id cfg)) => "glm-5.3")
  (check (cdr (assoc 'api-key cfg)) => "glm-token-xyz-123")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'base-url meta)) => "https://open.bigmodel.cn/api/anthropic")
    (check (cdr (assoc 'name meta)) => "GLM 5.3 (fixture)")))

;; 3. kimi-coding 端点由自建 fixture 钉死（baseUrl 归一化为 /coding/v1）
(let* ((args (parse-pi-args '("--provider" "kimi-coding")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_DIR)
              ("KIMI_API_KEY" . "kimi-key-xyz-123")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "kimi-coding")
  (check (cdr (assoc 'model-id cfg)) => "kimi-for-coding")
  (check (cdr (assoc 'api-key cfg)) => "kimi-key-xyz-123")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'base-url meta)) => "https://api.kimi.com/coding/v1")
    (check (cdr (assoc 'name meta)) => "Kimi K2.7 Code (fixture)")))

;; 4. 凭据由自建 fixture 的 auth.json 提供（字面量 key，不做 $VAR 插值）
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-3.8-flash")
  (check (cdr (assoc 'api-key cfg)) => "fixture-google-key-051")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'name meta)) => "Gemini 3.8 Flash (fixture)")
    (check (cdr (assoc 'base-url meta)) => "https://generativelanguage.googleapis.com/v1beta")))

;; 5. config-empty fixture 等价于「无用户配置」
(let* ((args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-3.8-flash")
  (check (cdr (assoc 'api-key cfg)) => #f)
  (check (cdr (assoc 'thinking-level cfg)) => "off"))

;; 6. 污染环境下解析结果不变（防回归：真实进程环境不得影响配置解析）
(let ((cfg (with-env-vars POLLUTION
             (lambda ()
               (resolve-pi-config (parse-pi-args '("--provider" "deepseek"))
                 `(("PI_CODING_AGENT_DIR" . ,CONFIG_DIR)
                   ("DEEPSEEK_API_KEY" . "ds-key-xyz-789")))
             ) ;lambda
           ) ;with-env-vars
       )) ;
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'model-id cfg)) => "deepseek-v4-pro")
  (check (cdr (assoc 'api-key cfg)) => "ds-key-xyz-789")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (cdr (assoc 'base-url meta)) => "https://api.deepseek.com/anthropic")
    (check (cdr (assoc 'name meta)) => "DeepSeek V4 Pro (fixture)")
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 384000)))

;; 6.1 污染环境下默认 provider 仍取自 fixture（不走真实 HOME 下的 ~/.pi/agent）
(let ((cfg (with-env-vars POLLUTION
             (lambda ()
               (resolve-pi-config (parse-pi-args '())
                 `(("PI_CODING_AGENT_DIR" . ,CONFIG_DIR)))
             ) ;lambda
           ) ;with-env-vars
       )) ;
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-3.8-flash")
  (check (cdr (assoc 'api-key cfg)) => "fixture-google-key-051"))

;; 6.2 污染环境下 config-empty fixture 仍等价于「无用户配置」
(let ((cfg (with-env-vars POLLUTION
             (lambda ()
               (resolve-pi-config (parse-pi-args '())
                 `(("PI_CODING_AGENT_DIR" . ,CONFIG_EMPTY_DIR)))
             ) ;lambda
           ) ;with-env-vars
       )) ;
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'api-key cfg)) => #f)
  (check (cdr (assoc 'thinking-level cfg)) => "off"))

(check-report)
