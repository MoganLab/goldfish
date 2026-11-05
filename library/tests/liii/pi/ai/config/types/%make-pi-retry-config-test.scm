;; %make-pi-retry-config
;; 构造重试策略配置 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-retry-config enabled max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
;;
;; 参数
;; ----
;; enabled : boolean?
;; 是否开启重试
;;
;; max-retries : integer?
;; 客户端级最大重试次数
;;
;; base-delay-ms : number?
;; 基础退避延迟毫秒数
;;
;; max-agent-delay-ms : number?
;; 单次退避允许的最大延迟毫秒数
;;
;; provider-max-retries : integer?
;; 服务商级最大重试次数
;;
;; 返回值
;; ----
;; pi-retry-config?
;; 重试策略配置 record 实例。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-retry-config #t 5 1500 45000 12)))
  (check-true (pi-retry-config? obj))
  (check (pi-retry-config-enabled obj) => #t)
  (check (pi-retry-config-max-retries obj) => 5)
  (check (pi-retry-config-base-delay-ms obj) => 1500)
  (check (pi-retry-config-max-agent-delay-ms obj) => 45000)
  (check (pi-retry-config-provider-max-retries obj) => 12)
) ;let

(check-report)
