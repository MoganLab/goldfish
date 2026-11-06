;; PI_OPENCODE_BUILTIN_MODELS
;; OpenCode 内置模型元数据定义列表。
;;
;; 语法
;; ----
;; PI_OPENCODE_BUILTIN_MODELS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含各内置模型配置信息的关联列表。

(import (liii check) (liii pi ai providers opencode))

(check-set-mode! 'report-failed)

(check-true (pair? PI_OPENCODE_BUILTIN_MODELS))
(check (length PI_OPENCODE_BUILTIN_MODELS) => 27)

(let ((deepseek (assoc "deepseek-v4.1-flash" (map (lambda (m) (cons (cdr (assoc "id" m)) m)) PI_OPENCODE_BUILTIN_MODELS))))
  (check-true (pair? deepseek))
  (check (cdr (assoc "name" (cdr deepseek))) => "DeepSeek V4.1 Flash")
  (check (cdr (assoc "baseUrl" (cdr deepseek))) => "https://opencode.ai/zen/go/v1"))

(check-report)
