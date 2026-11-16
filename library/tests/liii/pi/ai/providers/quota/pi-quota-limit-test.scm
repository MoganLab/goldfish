;; pi-quota-limit
;; 构造额度耗尽判定结果 record 对象。
;;
;; 语法
;; ----
;; (pi-quota-limit kind code [message])
;;
;; 参数
;; ----
;; kind : string?
;; 额度窗口类型，如 five-hour、weekly、quota
;;
;; code : string?
;; 命中的错误标识，如 1310、exceeded_current_quota_error
;;
;; message : string? 或 #f (可选，默认 #f)
;; 服务商官方消息原文，缺失时为 #f
;;
;; 返回值
;; ----
;; pi-quota-limit?
;; 额度耗尽判定结果 record 实例。

(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

(let ((obj (pi-quota-limit "weekly" "1310" "每周额度已用尽")))
  (check-true (pi-quota-limit? obj))
  (check (pi-quota-limit-kind obj) => "weekly")
  (check (pi-quota-limit-code obj) => "1310")
  (check (pi-quota-limit-message obj) => "每周额度已用尽")
) ;let

;; 省略可选参数 message 时默认 #f；支持关键字参数
(let ((obj-default (pi-quota-limit "five-hour" "1308"))
      (obj-kw (pi-quota-limit :kind "quota" :code "exceeded_current_quota_error" :message "limit"))
     ) ;
  (check-true (pi-quota-limit? obj-default))
  (check (pi-quota-limit-message obj-default) => #f)
  (check (pi-quota-limit-kind obj-kw) => "quota")
  (check (pi-quota-limit-code obj-kw) => "exceeded_current_quota_error")
  (check (pi-quota-limit-message obj-kw) => "limit")
) ;let

(check-report)
