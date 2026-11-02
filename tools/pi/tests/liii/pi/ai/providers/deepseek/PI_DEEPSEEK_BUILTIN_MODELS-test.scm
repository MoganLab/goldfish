;; PI_DEEPSEEK_BUILTIN_MODELS
;; DeepSeek 内置模型元数据定义列表。
;;
;; 语法
;; ----
;; PI_DEEPSEEK_BUILTIN_MODELS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含各内置模型配置信息的关联列表。

(import (liii check) (liii pi ai providers deepseek))

(check-set-mode! 'report-failed)

(check-true (pair? PI_DEEPSEEK_BUILTIN_MODELS))
(check (length PI_DEEPSEEK_BUILTIN_MODELS) => 3)

(check-report)
