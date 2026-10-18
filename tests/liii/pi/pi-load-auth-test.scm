(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-load-auth
;; 从 auth.json 读取 API key 认证凭据。
(let ((auth (pi-load-auth "tools/pi/tests/fixtures/default"
                          '(("TEST_ANTHROPIC_KEY" . "sk-ant-test-12345")))))
  (check (cdr (assoc "google" auth)) => "test-google-key-from-auth-json")
  (check (cdr (assoc "anthropic" auth)) => "sk-ant-test-12345")
  (check (assoc "kimi-coding" auth) => #f))

(check-catch 'pi-config-error
  (pi-load-auth "tools/pi/tests/fixtures/invalid_cmd"))

(check-report)
