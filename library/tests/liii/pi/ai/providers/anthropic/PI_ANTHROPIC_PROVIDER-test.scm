;; PI_ANTHROPIC_PROVIDER
;; Anthropic 官方 API 服务商基础配置定义。
;;
;; 语法
;; ----
;; PI_ANTHROPIC_PROVIDER
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含 api 协议、baseUrl 与 envKeys 环境变量列表的关联列表。

(import (liii check) (liii pi ai providers anthropic))

(check-set-mode! 'report-failed)

(check-true (pair? PI_ANTHROPIC_PROVIDER))
(check (cdr (assoc 'api PI_ANTHROPIC_PROVIDER)) => "anthropic-messages")
(check (cdr (assoc 'baseUrl PI_ANTHROPIC_PROVIDER))
  =>
  "https://api.anthropic.com"
) ;check
(check (cdr (assoc 'envKeys PI_ANTHROPIC_PROVIDER)) => '("ANTHROPIC_API_KEY"))

(check-report)
