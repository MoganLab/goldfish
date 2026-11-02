;; PI_ZAI_CODING_CN_BUILTIN_MODELS
;; 智谱 Z.AI GLM Coding 内置模型元数据定义列表。
;;
;; 语法
;; ----
;; PI_ZAI_CODING_CN_BUILTIN_MODELS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含各内置模型配置信息的关联列表。

(import (liii check) (liii pi ai providers zai-coding-cn))

(check-set-mode! 'report-failed)

(check-true (pair? PI_ZAI_CODING_CN_BUILTIN_MODELS))
(check (length PI_ZAI_CODING_CN_BUILTIN_MODELS) => 10)

(check-report)
