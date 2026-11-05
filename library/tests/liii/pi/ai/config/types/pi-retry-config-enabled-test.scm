;; pi-retry-config-enabled
;; 获取重试策略配置 record 的 enabled 字段。
;;
;; 语法
;; ----
;; (pi-retry-config-enabled obj)
;;
;; 参数
;; ----
;; obj : any?
;; 重试策略配置 record 实例。
;;
;; 返回值
;; ----
;; boolean?
;; enabled 字段的值。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-retry-config #t 5 1500 45000 12))
       (obj-b (%make-pi-retry-config #f 0 500 10000 3))
      ) ;
  (check (pi-retry-config-enabled obj-a) => #t)
  (check (pi-retry-config-enabled obj-b) => #f)
) ;let*

(check-report)
