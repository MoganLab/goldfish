;; PI_DEEPSEEK_PROVIDER
;; DeepSeek 服务商基础配置定义。
;;
;; 语法
;; ----
;; PI_DEEPSEEK_PROVIDER
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含 api 协议、baseUrl 与 envKeys 环境变量列表的关联列表。

(import (liii check) (liii pi ai providers deepseek))

(check-set-mode! 'report-failed)

(check-true (pair? PI_DEEPSEEK_PROVIDER))
(check (cdr (assoc 'api PI_DEEPSEEK_PROVIDER)) => "anthropic-messages")
(check (cdr (assoc 'baseUrl PI_DEEPSEEK_PROVIDER))
  =>
  "https://api.deepseek.com/anthropic"
) ;check

(check-report)
