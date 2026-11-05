;; openai-build-endpoint
;; 构造发往 OpenAI Chat Completions 端点的完整 URL。
;;
;; 语法
;; ----
;; (openai-build-endpoint base-url)
;;
;; 参数
;; ----
;; base-url : string? | #f
;; 服务商基础地址；缺省或 #f 时使用官方地址 https://api.openai.com/v1。
;;
;; 返回值
;; ----
;; string
;; 完整的 chat/completions 请求 URL。

(import (liii check) (liii pi ai api openai-completions))

(check-set-mode! 'report-failed)

(check (openai-build-endpoint "https://api.kimi.com/coding/v1")
  => "https://api.kimi.com/coding/v1/chat/completions")
(check (openai-build-endpoint "https://api.kimi.com/coding")
  => "https://api.kimi.com/coding/v1/chat/completions")
(check (openai-build-endpoint "http://localhost:8080/v1/")
  => "http://localhost:8080/v1/chat/completions")
(check (openai-build-endpoint "http://localhost:8080/chat/completions")
  => "http://localhost:8080/chat/completions")

;; 缺省值（协议层默认端点为 OpenAI 官方地址）
(check (openai-build-endpoint #f)
  => "https://api.openai.com/v1/chat/completions")

;; 仅主机名，无路径
(check (openai-build-endpoint "http://localhost:8080")
  => "http://localhost:8080/v1/chat/completions")

;; 主机名带尾斜杠
(check (openai-build-endpoint "http://localhost:8080/")
  => "http://localhost:8080/v1/chat/completions")

;; /v4 后缀
(check (openai-build-endpoint "https://proxy.example.com/v4")
  => "https://proxy.example.com/v4/chat/completions")

;; 包含 /paas/ 的路径
(check (openai-build-endpoint "https://open.bigmodel.cn/api/coding/paas/v4")
  => "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions")

;; 已带 /chat/completions 且带尾斜杠
(check (openai-build-endpoint "http://localhost:8080/chat/completions/")
  => "http://localhost:8080/chat/completions")

(check-report)
