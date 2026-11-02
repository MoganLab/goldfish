;; PI_GOOGLE_PROVIDER
;; Google Gemini 服务商基础配置定义。
;;
;; 语法
;; ----
;; PI_GOOGLE_PROVIDER
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含 api 协议、baseUrl 与 envKeys 环境变量列表的关联列表。

(import (liii check) (liii pi ai providers google))

(check-set-mode! 'report-failed)

(check-true (pair? PI_GOOGLE_PROVIDER))
(check (cdr (assoc 'api PI_GOOGLE_PROVIDER)) => "google-generative-ai")

(check-report)
