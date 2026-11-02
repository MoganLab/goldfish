;; pi-retry-config
;; 构造重试策略配置 record 对象。
;;
;; 语法
;; ----
;; (pi-retry-config)
;; (pi-retry-config enabled max-retries base-delay-ms max-agent-delay-ms)
;; (pi-retry-config enabled max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
;;
;; 参数
;; ----
;; enabled : boolean?
;; 是否开启重试。
;;
;; max-retries : integer?
;; 最大重试次数。
;;
;; base-delay-ms : number?
;; 基础延迟毫秒数。
;;
;; max-agent-delay-ms : number?
;; 最大延迟毫秒数。
;;
;; provider-max-retries : integer?
;; 可选，服务商级别最大重试次数。
;;
;; 返回值
;; ----
;; pi-retry-config?
;; 重试配置 record 实例。

(import (liii check) (liii pi ai config record))

(check-set-mode! 'report-failed)

(let ((cfg (pi-retry-config #t 5 1500 45000 12)))
  (check-true (pi-retry-config? cfg))
  (check (pi-retry-config-enabled cfg) => #t)
  (check (pi-retry-config-max-retries cfg) => 5)
  (check (pi-retry-config-base-delay-ms cfg) => 1500)
  (check (pi-retry-config-max-agent-delay-ms cfg) => 45000)
  (check (pi-retry-config-provider-max-retries cfg) => 12)
) ;let

(check-report)
