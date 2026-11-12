;; PI_ALL_IN_ONE_PROVIDER
;; all-in-one 链式调度元服务商基础配置定义。
;;
;; 语法
;; ----
;; PI_ALL_IN_ONE_PROVIDER
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含 api 协议与 envKeys 环境变量列表的关联列表。

(import (liii check) (liii pi ai providers all-in-one))

(check-set-mode! 'report-failed)

(check-true (pair? PI_ALL_IN_ONE_PROVIDER))
(check (cdr (assoc 'api PI_ALL_IN_ONE_PROVIDER)) => "all-in-one")

(check-report)
