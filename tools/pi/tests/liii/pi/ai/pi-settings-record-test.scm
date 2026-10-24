(import (liii check) (liii pi ai config record) (liii pi ai event))

(check-set-mode! 'report-failed)

;; pi-settings-record-test.scm
;; 验证 pi-settings record 构造器、谓词与访问器，以及对 assoc 的平滑兼容。

(define test-retry
  (pi-retry-config #t 5 1500 45000))

(check-true (pi-retry-config? test-retry))
(check (pi-retry-config-enabled test-retry) => #t)
(check (pi-retry-config-max-retries test-retry) => 5)
(check (pi-retry-config-base-delay-ms test-retry) => 1500)
(check (pi-retry-config-max-agent-delay-ms test-retry) => 45000)
(check (pi-retry-config-provider-max-retries test-retry) => #f)

;; assoc 兼容访问 retry record
(check (cdr (assoc 'enabled test-retry)) => #t)
(check (cdr (assoc "enabled" test-retry)) => #t)
(check (cdr (assoc 'max-retries test-retry)) => 5)
(check (cdr (assoc "max-retries" test-retry)) => 5)
(check (cdr (assoc 'base-delay-ms test-retry)) => 1500)
(check (cdr (assoc "base-delay-ms" test-retry)) => 1500)
(check (cdr (assoc 'max-agent-delay-ms test-retry)) => 45000)
(check (cdr (assoc "max-agent-delay-ms" test-retry)) => 45000)
(check (cdr (assoc 'provider-max-retries test-retry)) => #f)
(check (cdr (assoc "provider-max-retries" test-retry)) => #f)

;; 指定 provider 级预算（第 5 个构造参数）
(define test-retry-provider (pi-retry-config #t 5 1500 45000 12))
(check (pi-retry-config-provider-max-retries test-retry-provider) => 12)
(check (cdr (assoc 'provider-max-retries test-retry-provider)) => 12)
(check (cdr (assoc "provider-max-retries" test-retry-provider)) => 12)

(define test-settings
  (pi-settings "google" "gemini-3.8-flash" "medium" "http://proxy:8080" test-retry '("read" "bash")))

(check-true (pi-settings? test-settings))
(check (pi-settings-default-provider test-settings) => "google")
(check (pi-settings-default-model test-settings) => "gemini-3.8-flash")
(check (pi-settings-default-thinking-level test-settings) => "medium")
(check (pi-settings-http-proxy test-settings) => "http://proxy:8080")
(check-true (pi-retry-config? (pi-settings-retry test-settings)))
(check (pi-settings-default-tools test-settings) => '("read" "bash"))

;; assoc 兼容访问 settings record
(check (cdr (assoc 'default-provider test-settings)) => "google")
(check (cdr (assoc "default-provider" test-settings)) => "google")
(check (cdr (assoc 'default-model test-settings)) => "gemini-3.8-flash")
(check (cdr (assoc "default-model" test-settings)) => "gemini-3.8-flash")
(check (cdr (assoc 'default-thinking-level test-settings)) => "medium")
(check (cdr (assoc "default-thinking-level" test-settings)) => "medium")
(check (cdr (assoc 'http-proxy test-settings)) => "http://proxy:8080")
(check (cdr (assoc "http-proxy" test-settings)) => "http://proxy:8080")
(check (cdr (assoc 'retry test-settings)) => test-retry)
(check (cdr (assoc "retry" test-settings)) => test-retry)
(check (cdr (assoc 'default-tools test-settings)) => '("read" "bash"))
(check (cdr (assoc "default-tools" test-settings)) => '("read" "bash"))

(check-report)
