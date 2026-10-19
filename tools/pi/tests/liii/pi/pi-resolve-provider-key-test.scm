(import (liii check) (liii pi) (liii path))

(check-set-mode! 'report-failed)

;; pi-resolve-provider-key
;; 解析指定 provider 的有效 API 密钥。环境变量优先于 auth.json 存储条目。

;; google 测试
(let ((auth-with-google '(("google" . "key-from-auth")))
      (auth-empty '())
      (env-with-gemini '(("GEMINI_API_KEY" . "gemini-env-key-999"))))
  ;; 环境变量优先于存储条目
  (check (pi-resolve-provider-key "google" auth-with-google env-with-gemini)
    => "gemini-env-key-999")
  ;; 环境变量为空或仅空白字符时回退到存储条目
  (check (pi-resolve-provider-key "google" auth-with-google '(("GEMINI_API_KEY" . "")))
    => "key-from-auth")
  (check (pi-resolve-provider-key "google" auth-with-google '(("GEMINI_API_KEY" . "   ")))
    => "key-from-auth")
  ;; 环境变量未设置时回退到存储条目
  (check (pi-resolve-provider-key "google" auth-with-google '())
    => "key-from-auth")
  ;; 存储条目不存在时，探测环境变量 GEMINI_API_KEY
  (check (pi-resolve-provider-key "google" auth-empty env-with-gemini)
    => "gemini-env-key-999")
  ;; 符号 provider 支持
  (check (pi-resolve-provider-key 'google auth-with-google env-with-gemini)
    => "gemini-env-key-999")
  (check (pi-resolve-provider-key 'google auth-with-google '())
    => "key-from-auth")
  ;; 均未配置时返回 #f
  (check (pi-resolve-provider-key "google" auth-empty '())
    => #f)
  (check (pi-resolve-provider-key "unknown-provider" auth-empty '())
    => #f)
  ;; 未知 provider 但存在存储条目时返回存储条目
  (check (pi-resolve-provider-key "custom-provider" '(("custom-provider" . "custom-val")) '())
    => "custom-val"))

;; kimi-coding 测试
(let ((auth-with-kimi '(("kimi-coding" . "kimi-auth-key-123")))
      (auth-empty '())
      (env-with-kimi '(("KIMI_API_KEY" . "kimi-env-key-456"))))
  ;; 环境变量优先于存储条目（差距清单 #10：支持通过环境变量切换 Key）
  (check (pi-resolve-provider-key "kimi-coding" auth-with-kimi env-with-kimi)
    => "kimi-env-key-456")
  ;; 环境变量为空或空白时回退到存储条目
  (check (pi-resolve-provider-key "kimi-coding" auth-with-kimi '(("KIMI_API_KEY" . "")))
    => "kimi-auth-key-123")
  (check (pi-resolve-provider-key "kimi-coding" auth-with-kimi '(("KIMI_API_KEY" . "  ")))
    => "kimi-auth-key-123")
  ;; 环境变量未设置时回退到存储条目
  (check (pi-resolve-provider-key "kimi-coding" auth-with-kimi '())
    => "kimi-auth-key-123")
  ;; 存储条目不存在时，探测环境变量
  (check (pi-resolve-provider-key "kimi-coding" auth-empty env-with-kimi)
    => "kimi-env-key-456")
  ;; 符号 provider 支持
  (check (pi-resolve-provider-key 'kimi-coding auth-with-kimi env-with-kimi)
    => "kimi-env-key-456")
  (check (pi-resolve-provider-key 'kimi-coding auth-with-kimi '())
    => "kimi-auth-key-123")
  ;; 均未配置时返回 #f
  (check (pi-resolve-provider-key "kimi-coding" auth-empty '())
    => #f))

;; anthropic 测试
(let ((auth-with-ant '(("anthropic" . "ant-auth-key-000")))
      (auth-empty '())
      (env-with-ant-api '(("ANTHROPIC_API_KEY" . "ant-api-env-111")))
      (env-with-ant-tok '(("ANTHROPIC_AUTH_TOKEN" . "ant-tok-env-222"))))
  ;; 环境变量 ANTHROPIC_API_KEY 优先于存储条目
  (check (pi-resolve-provider-key "anthropic" auth-with-ant env-with-ant-api)
    => "ant-api-env-111")
  ;; 环境变量 ANTHROPIC_AUTH_TOKEN 优先于存储条目
  (check (pi-resolve-provider-key "anthropic" auth-with-ant env-with-ant-tok)
    => "ant-tok-env-222")
  ;; ANTHROPIC_API_KEY 优先于 ANTHROPIC_AUTH_TOKEN
  (check (pi-resolve-provider-key "anthropic"
                                  auth-empty
                                  '(("ANTHROPIC_API_KEY" . "ant-api-env-111")
                                    ("ANTHROPIC_AUTH_TOKEN" . "ant-tok-env-222")))
    => "ant-api-env-111")
  ;; 环境变量为空时回退到存储条目
  (check (pi-resolve-provider-key "anthropic" auth-with-ant '(("ANTHROPIC_API_KEY" . "")))
    => "ant-auth-key-000")
  (check (pi-resolve-provider-key "anthropic" auth-with-ant '())
    => "ant-auth-key-000")
  ;; 存储条目不存在时，探测环境变量
  (check (pi-resolve-provider-key "anthropic" auth-empty env-with-ant-api)
    => "ant-api-env-111")
  (check (pi-resolve-provider-key "anthropic" auth-empty env-with-ant-tok)
    => "ant-tok-env-222")
  ;; 符号 provider 支持
  (check (pi-resolve-provider-key 'anthropic auth-with-ant env-with-ant-api)
    => "ant-api-env-111")
  ;; 均未配置时返回 #f
  (check (pi-resolve-provider-key "anthropic" auth-empty '())
    => #f))

;; glm 测试
(let ((auth-with-glm '(("glm" . "glm-auth-key-789")))
      (auth-empty '())
      (env-with-ant-token '(("ANTHROPIC_AUTH_TOKEN" . "glm-token-abc")))
      (env-with-glm-key '(("GLM_API_KEY" . "glm-key-def")))
      (env-with-ant-key '(("ANTHROPIC_API_KEY" . "ant-key-ghi"))))
  ;; 环境变量优先于存储条目
  (check (pi-resolve-provider-key "glm" auth-with-glm env-with-ant-token)
    => "glm-token-abc")
  (check (pi-resolve-provider-key "glm" auth-with-glm env-with-glm-key)
    => "glm-key-def")
  (check (pi-resolve-provider-key "glm" auth-with-glm env-with-ant-key)
    => "ant-key-ghi")
  ;; 环境变量优先级: ANTHROPIC_AUTH_TOKEN > GLM_API_KEY > ANTHROPIC_API_KEY
  (check (pi-resolve-provider-key "glm"
                                  auth-empty
                                  '(("ANTHROPIC_AUTH_TOKEN" . "glm-token-abc")
                                    ("GLM_API_KEY" . "glm-key-def")
                                    ("ANTHROPIC_API_KEY" . "ant-key-ghi")))
    => "glm-token-abc")
  (check (pi-resolve-provider-key "glm"
                                  auth-empty
                                  '(("GLM_API_KEY" . "glm-key-def")
                                    ("ANTHROPIC_API_KEY" . "ant-key-ghi")))
    => "glm-key-def")
  ;; 环境变量为空时回退到存储条目
  (check (pi-resolve-provider-key "glm" auth-with-glm '(("GLM_API_KEY" . "")))
    => "glm-auth-key-789")
  (check (pi-resolve-provider-key "glm" auth-with-glm '())
    => "glm-auth-key-789")
  ;; 存储条目不存在时，探测环境变量
  (check (pi-resolve-provider-key "glm" auth-empty env-with-ant-token)
    => "glm-token-abc")
  (check (pi-resolve-provider-key "glm" auth-empty env-with-glm-key)
    => "glm-key-def")
  (check (pi-resolve-provider-key "glm" auth-empty env-with-ant-key)
    => "ant-key-ghi")
  ;; 符号 provider 支持
  (check (pi-resolve-provider-key 'glm auth-with-glm env-with-glm-key)
    => "glm-key-def")
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
  ;; 环境变量 DEEPSEEK_API_KEY 优先于存储条目
  (check (pi-resolve-provider-key "deepseek" auth-with-ds env-with-ds-key)
    => "ds-env-key-222")
  ;; 环境变量为空时回退到存储条目
  (check (pi-resolve-provider-key "deepseek" auth-with-ds '(("DEEPSEEK_API_KEY" . "")))
    => "ds-auth-key-111")
  (check (pi-resolve-provider-key "deepseek" auth-with-ds env-empty)
    => "ds-auth-key-111")
  ;; 环境变量 DEEPSEEK_API_KEY（auth 为空时）
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
  ;; 符号 provider 支持
  (check (pi-resolve-provider-key 'deepseek auth-with-ds env-with-ds-key)
    => "ds-env-key-222")
  ;; 均无配置返回 #f
  (check (pi-resolve-provider-key "deepseek" auth-empty env-empty)
    => #f))

;; pi-credential record 兼容测试
(let ((cred-google (pi-credential "google" "record-key-google"))
      (env-with-gemini '(("GEMINI_API_KEY" . "gemini-env-override"))))
  ;; 环境变量优先于 record 形式的存储条目
  (check (pi-resolve-provider-key "google" (list cred-google) env-with-gemini)
    => "gemini-env-override")
  ;; 环境变量未设置时使用 record 存储条目
  (check (pi-resolve-provider-key "google" (list cred-google) '())
    => "record-key-google"))

(check-report)
