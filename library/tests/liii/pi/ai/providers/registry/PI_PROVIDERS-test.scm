;; PI_PROVIDERS
;; 全部内置服务商配置注册表。
;;
;; 语法
;; ----
;; PI_PROVIDERS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 服务商 id 到配置关联表的注册表列表。

(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)

(check-true (pair? PI_PROVIDERS))
(check-true (pair? (assoc "deepseek" PI_PROVIDERS)))
(check-true (pair? (assoc "google" PI_PROVIDERS)))

(check-report)
