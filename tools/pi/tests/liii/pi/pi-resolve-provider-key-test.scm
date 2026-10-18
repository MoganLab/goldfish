(import (liii check) (liii pi) (liii path))

(check-set-mode! 'report-failed)

;; pi-resolve-provider-key
;; 解析指定 provider 的有效 API 密钥。
(let ((auth-with-google '(("google" . "key-from-auth")))
      (auth-empty '())
      (env-with-gemini '(("GEMINI_API_KEY" . "gemini-env-key-999"))))
  ;; 存储条目优先
  (check (pi-resolve-provider-key "google" auth-with-google env-with-gemini)
    => "key-from-auth")
  ;; 存储条目不存在时，探测环境变量 GEMINI_API_KEY
  (check (pi-resolve-provider-key "google" auth-empty env-with-gemini)
    => "gemini-env-key-999")
  ;; 均未配置时返回 #f
  (check (pi-resolve-provider-key "google" auth-empty '())
    => #f)
  (check (pi-resolve-provider-key "unknown-provider" auth-empty '())
    => #f))

;; kimi-coding 测试
(let ((auth-with-kimi '(("kimi-coding" . "kimi-auth-key-123")))
      (auth-empty '())
      (env-with-kimi '(("KIMI_API_KEY" . "kimi-env-key-456"))))
  (check (pi-resolve-provider-key "kimi-coding" auth-with-kimi env-with-kimi)
    => "kimi-auth-key-123")
  (check (pi-resolve-provider-key "kimi-coding" auth-empty env-with-kimi)
    => "kimi-env-key-456")
  (check (pi-resolve-provider-key "kimi-coding" auth-empty '())
    => #f))

;; glm 测试
(let ((auth-with-glm '(("glm" . "glm-auth-key-789")))
      (auth-empty '())
      (env-with-ant-token '(("ANTHROPIC_AUTH_TOKEN" . "glm-token-abc")))
      (env-with-glm-key '(("GLM_API_KEY" . "glm-key-def")))
      (env-with-ant-key '(("ANTHROPIC_API_KEY" . "ant-key-ghi"))))
  ;; 存储条目优先
  (check (pi-resolve-provider-key "glm" auth-with-glm env-with-ant-token)
    => "glm-auth-key-789")
  ;; 存储条目不存在时，探测环境变量 ANTHROPIC_AUTH_TOKEN / GLM_API_KEY / ANTHROPIC_API_KEY
  (check (pi-resolve-provider-key "glm" auth-empty env-with-ant-token)
    => "glm-token-abc")
  (check (pi-resolve-provider-key "glm" auth-empty env-with-glm-key)
    => "glm-key-def")
  (check (pi-resolve-provider-key "glm" auth-empty env-with-ant-key)
    => "ant-key-ghi")
  ;; 均未配置时返回 #f
  (check (pi-resolve-provider-key "glm" auth-empty '())
    => #f))

;; deepseek 测试
(let* ((auth-with-ds '(("deepseek" . "ds-auth-key-111")))
       (auth-empty '())
       (claude-home
         (cond ((file-exists? "tools/pi/tests/fixtures/deepseek/claude_home")
                "tools/pi/tests/fixtures/deepseek/claude_home")
               ((file-exists? "tests/fixtures/deepseek/claude_home")
                "tests/fixtures/deepseek/claude_home")
               (else "../../fixtures/deepseek/claude_home")))
       (ds-home
         (cond ((file-exists? "tools/pi/tests/fixtures/deepseek/ds_home")
                "tools/pi/tests/fixtures/deepseek/ds_home")
               ((file-exists? "tests/fixtures/deepseek/ds_home")
                "tests/fixtures/deepseek/ds_home")
               (else "../../fixtures/deepseek/ds_home")))
       (env-with-ds-key '(("DEEPSEEK_API_KEY" . "ds-env-key-222") ("HOME" . "/non-existent")))
       (env-with-claude `(("HOME" . ,claude-home)))
       (env-with-ds-home `(("HOME" . ,ds-home)))
       (env-with-ant-token '(("ANTHROPIC_AUTH_TOKEN" . "ant-token-333") ("HOME" . "/non-existent")))
       (env-empty '(("HOME" . "/non-existent"))))
  ;; 存储条目优先
  (check (pi-resolve-provider-key "deepseek" auth-with-ds env-with-ds-key)
    => "ds-auth-key-111")
  ;; 环境变量 DEEPSEEK_API_KEY
  (check (pi-resolve-provider-key "deepseek" auth-empty env-with-ds-key)
    => "ds-env-key-222")
  ;; ~/.claude/settings.json.deepseek 发现
  (check (pi-resolve-provider-key "deepseek" auth-empty env-with-claude)
    => "claude-deepseek-token-mock-123")
  ;; ~/.deepseek.json 发现
  (check (pi-resolve-provider-key "deepseek" auth-empty env-with-ds-home)
    => "deepseek-json-key-mock-456")
  ;; ANTHROPIC_AUTH_TOKEN 兜底
  (check (pi-resolve-provider-key "deepseek" auth-empty env-with-ant-token)
    => "ant-token-333")
  ;; 均无配置返回 #f
  (check (pi-resolve-provider-key "deepseek" auth-empty env-empty)
    => #f))

(check-report)
