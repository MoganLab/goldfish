;; pi-settings
;; 构造客户端全局偏好配置 record 对象。
;;
;; 语法
;; ----
;; (pi-settings)
;; (pi-settings default-provider default-model default-thinking-level http-proxy retry default-tools)
;;
;; 参数
;; ----
;; default-provider : string?
;; 默认服务商。
;;
;; default-model : string?
;; 默认模型。
;;
;; default-thinking-level : string?
;; 默认思考级别。
;;
;; http-proxy : string? 或 #f
;; 代理地址。
;;
;; retry : any?
;; 重试配置。
;;
;; default-tools : list?
;; 默认开启的工具列表。
;;
;; 返回值
;; ----
;; pi-settings?
;; 全局偏好配置 record 实例。

(import (liii check) (liii pi ai config record))

(check-set-mode! 'report-failed)

(let* ((ret (pi-retry-config #t 3 1000 30000))
       (s (pi-settings "google" "gemini-3.8-flash" "medium" #f ret '("read")))
      ) ;
  (check-true (pi-settings? s))
  (check (pi-settings-default-provider s) => "google")
  (check (pi-settings-default-model s) => "gemini-3.8-flash")
  (check (pi-settings-default-thinking-level s) => "medium")
) ;let*

(check-report)
