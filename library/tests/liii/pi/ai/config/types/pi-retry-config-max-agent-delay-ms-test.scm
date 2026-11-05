;; pi-retry-config-max-agent-delay-ms
;; 获取重试策略配置 record 的 max-agent-delay-ms 字段。
;;
;; 语法
;; ----
;; (pi-retry-config-max-agent-delay-ms obj)
;;
;; 参数
;; ----
;; obj : any?
;; 重试策略配置 record 实例。
;;
;; 返回值
;; ----
;; number?
;; max-agent-delay-ms 字段的值。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-retry-config #t 5 1500 45000 12))
       (obj-b (%make-pi-retry-config #f 0 500 10000 3))
      ) ;
  (check (pi-retry-config-max-agent-delay-ms obj-a) => 45000)
  (check (pi-retry-config-max-agent-delay-ms obj-b) => 10000)
) ;let*

(check-report)
