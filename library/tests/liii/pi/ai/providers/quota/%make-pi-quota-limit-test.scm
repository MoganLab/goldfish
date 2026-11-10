;; %make-pi-quota-limit
;; 构造额度耗尽判定结果 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-quota-limit kind code message)
;;
;; 参数
;; ----
;; kind : string?
;; 额度窗口类型，如 five-hour、weekly、quota
;;
;; code : string?
;; 命中的错误标识，如 1310、exceeded_current_quota_error
;;
;; message : string? 或 #f
;; 服务商官方消息原文，缺失时为 #f
;;
;; 返回值
;; ----
;; pi-quota-limit?
;; 额度耗尽判定结果 record 实例。

(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-quota-limit "weekly" "1310" "每周额度已用尽")))
  (check-true (pi-quota-limit? obj))
  (check (pi-quota-limit-kind obj) => "weekly")
  (check (pi-quota-limit-code obj) => "1310")
  (check (pi-quota-limit-message obj) => "每周额度已用尽")
) ;let

(check-report)
