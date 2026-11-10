;; PI_OLLAMA_PROVIDER
;; Ollama 服务商基础配置定义。
;;
;; 语法
;; ----
;; PI_OLLAMA_PROVIDER
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含 api 协议、baseUrl 与 envKeys 环境变量列表的关联列表。

(import (liii check) (liii pi ai providers ollama))

(check-set-mode! 'report-failed)

(check-true (pair? PI_OLLAMA_PROVIDER))
(check (cdr (assoc 'api PI_OLLAMA_PROVIDER)) => "openai-completions")
(check (cdr (assoc 'baseUrl PI_OLLAMA_PROVIDER)) => "http://localhost:11434/v1")

(check-report)
