(import (liii check) (liii pi ai kimi))

(check-set-mode! 'report-failed)

;; kimi-build-endpoint
(check (kimi-build-endpoint "https://api.kimi.com/coding/v1")
  => "https://api.kimi.com/coding/v1/chat/completions")
(check (kimi-build-endpoint "https://api.kimi.com/coding")
  => "https://api.kimi.com/coding/v1/chat/completions")
(check (kimi-build-endpoint "http://localhost:8080/v1/")
  => "http://localhost:8080/v1/chat/completions")
(check (kimi-build-endpoint "http://localhost:8080/chat/completions")
  => "http://localhost:8080/chat/completions")

(check-report)
