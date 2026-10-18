(import (liii check) (liii pi))

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

(check-report)
