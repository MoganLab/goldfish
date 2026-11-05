;; pi-provider-record
;; 获取指定服务商的完整配置关联列表。
;;
;; 语法
;; ----
;; (pi-provider-record provider)
;;
;; 参数
;; ----
;; provider : string? 或 symbol?
;; 服务商标识符。
;;
;; 返回值
;; ----
;; alist? 或 #f
;; 包含 api、baseUrl、envKeys 的配置关联表；未注册返回 #f。

(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)

(check (pi-provider-record "unknown-provider") => #f)
(check (pair? (pi-provider-record "google")) => #t)

(check-report)
