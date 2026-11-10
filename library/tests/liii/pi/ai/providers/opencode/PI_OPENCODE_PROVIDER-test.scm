;; PI_OPENCODE_PROVIDER
;; OpenCode 服务商基础配置定义。
;;
;; 语法
;; ----
;; PI_OPENCODE_PROVIDER
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含 api 协议、baseUrl 与 envKeys 环境变量列表的关联列表。

(import (liii check) (liii pi ai providers opencode))

(check-set-mode! 'report-failed)

(check-true (pair? PI_OPENCODE_PROVIDER))
(check (cdr (assoc 'api PI_OPENCODE_PROVIDER)) => "openai-completions")
(check (cdr (assoc 'baseUrl PI_OPENCODE_PROVIDER)) => "https://opencode.ai/zen/go/v1")
(check (cdr (assoc 'envKeys PI_OPENCODE_PROVIDER)) => '("OPENCODE_API_KEY" "OPENCODE_GO_API_KEY"))

(check-report)
