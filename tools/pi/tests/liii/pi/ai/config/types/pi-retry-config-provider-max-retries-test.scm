;; pi-retry-config-provider-max-retries
;; 获取重试策略配置 record 的 provider-max-retries 字段。
;;
;; 语法
;; ----
;; (pi-retry-config-provider-max-retries obj)
;;
;; 参数
;; ----
;; obj : any?
;; 重试策略配置 record 实例。
;;
;; 返回值
;; ----
;; integer?
;; provider-max-retries 字段的值。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-retry-config #t 5 1500 45000 12))
       (obj-b (%make-pi-retry-config #f 0 500 10000 3))
      ) ;
  (check (pi-retry-config-provider-max-retries obj-a) => 12)
  (check (pi-retry-config-provider-max-retries obj-b) => 3)
) ;let*

(check-report)
