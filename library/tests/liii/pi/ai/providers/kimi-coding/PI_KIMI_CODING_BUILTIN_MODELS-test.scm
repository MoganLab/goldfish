;; PI_KIMI_CODING_BUILTIN_MODELS
;; Kimi Coding 内置模型元数据定义列表。
;;
;; 语法
;; ----
;; PI_KIMI_CODING_BUILTIN_MODELS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含各内置模型配置信息的关联列表。

(import (liii check) (liii pi ai providers kimi-coding))

(check-set-mode! 'report-failed)

(check-true (pair? PI_KIMI_CODING_BUILTIN_MODELS))
(check (length PI_KIMI_CODING_BUILTIN_MODELS) => 5)

(check-report)
