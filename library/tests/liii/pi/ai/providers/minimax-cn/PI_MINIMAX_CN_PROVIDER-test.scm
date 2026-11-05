;; PI_MINIMAX_CN_PROVIDER
;; MiniMax 国内版服务商基础配置定义。
;;
;; 语法
;; ----
;; PI_MINIMAX_CN_PROVIDER
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含 api 协议、baseUrl 与 envKeys 环境变量列表的关联列表。

(import (liii check) (liii pi ai providers minimax-cn))

(check-set-mode! 'report-failed)

(check-true (pair? PI_MINIMAX_CN_PROVIDER))
(check (cdr (assoc 'api PI_MINIMAX_CN_PROVIDER)) => "anthropic-messages")

(check-report)
