;; PI_OLLAMA_BUILTIN_MODELS
;; Ollama 服务商内置模型列表。
;;
;; 语法
;; ----
;; PI_OLLAMA_BUILTIN_MODELS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; Ollama 内置模型元数据列表。

(import (liii check) (liii pi ai providers ollama))

(check-set-mode! 'report-failed)

(check-true (pair? PI_OLLAMA_BUILTIN_MODELS))
(check (length PI_OLLAMA_BUILTIN_MODELS) => 3)

(let ((first-model (car PI_OLLAMA_BUILTIN_MODELS)))
  (check (cdr (assoc "id" first-model)) => "qwen2.5-coder:latest")
  (check (cdr (assoc "baseUrl" first-model)) => "http://localhost:11434/v1")
) ;let

(check-report)
