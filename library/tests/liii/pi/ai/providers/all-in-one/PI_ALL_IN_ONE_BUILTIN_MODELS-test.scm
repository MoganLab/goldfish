;; PI_ALL_IN_ONE_BUILTIN_MODELS
;; all-in-one 内置链式伪模型元数据定义列表。
;;
;; 语法
;; ----
;; PI_ALL_IN_ONE_BUILTIN_MODELS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含 coding-fast 与 coding-high-quality 内置伪模型配置信息的关联列表。

(import (liii check) (liii pi ai providers all-in-one))

(check-set-mode! 'report-failed)

(check-true (pair? PI_ALL_IN_ONE_BUILTIN_MODELS))
(check (length PI_ALL_IN_ONE_BUILTIN_MODELS) => 2)

(check-report)
