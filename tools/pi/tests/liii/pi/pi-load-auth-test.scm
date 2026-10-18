(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-load-auth
;; 从 auth.json 读取 API key 认证凭据。
(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"
  ) ;if
) ;define

(define invalid-cmd-dir
  (if (file-exists? "tests/fixtures/invalid_cmd")
    "tests/fixtures/invalid_cmd"
    "tools/pi/tests/fixtures/invalid_cmd"
  ) ;if
) ;define

(let ((auth (pi-load-auth fixture-dir
                          '(("TEST_ANTHROPIC_KEY" . "sk-ant-test-12345")))))
  (check-true (pi-credential? (car auth)))
  (check (pi-credential-provider (car auth)) => "google")
  (check (pi-credential-key (car auth)) => "test-google-key-from-auth-json")
  (check (cdr (assoc "google" auth)) => "test-google-key-from-auth-json")
  (check (cdr (assoc "anthropic" auth)) => "sk-ant-test-12345")
  (check (assoc "kimi-coding" auth) => #f))

(check-catch 'pi-config-error
  (pi-load-auth invalid-cmd-dir))

(check-report)
