;; PI_ZAI_CODING_CN_PROVIDER
;; 智谱 Z.AI GLM Coding 服务商基础配置定义。
;;
;; 语法
;; ----
;; PI_ZAI_CODING_CN_PROVIDER
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含 api 协议、baseUrl 与 envKeys 环境变量列表的关联列表。

(import (liii check) (liii pi ai providers zai-coding-cn))

(check-set-mode! 'report-failed)

(check-true (pair? PI_ZAI_CODING_CN_PROVIDER))
(check (cdr (assoc 'api PI_ZAI_CODING_CN_PROVIDER)) => "openai-completions")

(check-report)
