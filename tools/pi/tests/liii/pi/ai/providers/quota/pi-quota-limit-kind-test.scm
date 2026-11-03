;; pi-quota-limit-kind
;; 获取额度耗尽判定结果 record 的 kind 字段。
;;
;; 语法
;; ----
;; (pi-quota-limit-kind obj)
;;
;; 参数
;; ----
;; obj : any?
;; 额度耗尽判定结果 record 实例。
;;
;; 返回值
;; ----
;; string?
;; kind 字段的值。

(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-quota-limit "weekly" "1310" "每周额度已用尽"))
       (obj-b (%make-pi-quota-limit "five-hour" "exceeded_current_quota_error" #f))
      ) ;
  (check (pi-quota-limit-kind obj-a) => "weekly")
  (check (pi-quota-limit-kind obj-b) => "five-hour")
) ;let*

(check-report)
