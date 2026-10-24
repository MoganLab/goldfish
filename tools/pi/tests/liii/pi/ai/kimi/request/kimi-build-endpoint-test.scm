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

;; 缺省值
(check (kimi-build-endpoint #f)
  => "https://api.kimi.com/coding/v1/chat/completions")

;; 仅主机名，无路径
(check (kimi-build-endpoint "http://localhost:8080")
  => "http://localhost:8080/v1/chat/completions")

;; 主机名带尾斜杠
(check (kimi-build-endpoint "http://localhost:8080/")
  => "http://localhost:8080/v1/chat/completions")

;; /v4 后缀
(check (kimi-build-endpoint "https://proxy.example.com/v4")
  => "https://proxy.example.com/v4/chat/completions")

;; 包含 /paas/ 的路径
(check (kimi-build-endpoint "https://open.bigmodel.cn/api/coding/paas/v4")
  => "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions")

;; 已带 /chat/completions 且带尾斜杠
(check (kimi-build-endpoint "http://localhost:8080/chat/completions/")
  => "http://localhost:8080/chat/completions")

(check-report)
