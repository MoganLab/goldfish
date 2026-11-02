;; anthropic-build-endpoint
;; 构造发往 Anthropic Messages API 端的完整 URL。
;;
;; 语法
;; ----
;; (anthropic-build-endpoint base-url)
;;
;; 参数
;; ----
;; base-url : string? | #f
;; 服务商基础地址；缺省或 #f 时使用官方地址 https://api.anthropic.com。
;;
;; 返回值
;; ----
;; string
;; 形如 <base-url>/v1/messages 的请求 URL。

(import (liii check)
  (liii pi ai api anthropic-messages request)
) ;import

(check-set-mode! 'report-failed)

;; 官方端点
(check (anthropic-build-endpoint "https://api.anthropic.com")
  => "https://api.anthropic.com/v1/messages")

;; 缺省值
(check (anthropic-build-endpoint #f)
  => "https://api.anthropic.com/v1/messages")

;; 带尾斜杠
(check (anthropic-build-endpoint "https://api.anthropic.com/")
  => "https://api.anthropic.com/v1/messages")

;; glm 的 Anthropic 兼容端点
(check (anthropic-build-endpoint "https://open.bigmodel.cn/api/anthropic")
  => "https://open.bigmodel.cn/api/anthropic/v1/messages")

;; deepseek 的 Anthropic 兼容端点
(check (anthropic-build-endpoint "https://api.deepseek.com/anthropic")
  => "https://api.deepseek.com/anthropic/v1/messages")

(check-report)
