;; pi-quota-limit-code
;; 获取额度耗尽判定结果 record 的 code 字段。
;;
;; 语法
;; ----
;; (pi-quota-limit-code obj)
;;
;; 参数
;; ----
;; obj : any?
;; 额度耗尽判定结果 record 实例。
;;
;; 返回值
;; ----
;; string?
;; code 字段的值。

(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-quota-limit "weekly" "1310" "每周额度已用尽"))
       (obj-b (%make-pi-quota-limit "five-hour" "exceeded_current_quota_error" #f))
      ) ;
  (check (pi-quota-limit-code obj-a) => "1310")
  (check (pi-quota-limit-code obj-b) => "exceeded_current_quota_error")
) ;let*

(check-report)
