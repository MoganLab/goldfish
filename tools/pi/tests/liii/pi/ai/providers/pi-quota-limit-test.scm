(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

;; pi-quota-limit
;;
;; <pi-quota-limit> record 的构造与访问器（pi-quota-limit? / -kind / -code / -message）。
;; 额度耗尽判定结果：kind（额度窗口类型）、code（错误标识）、message（官方消息原文）。

(define limit (%make-pi-quota-limit "weekly" "1310" "每周额度已用尽"))

;; 1. 谓词
(check-true (pi-quota-limit? limit))
(check-false (pi-quota-limit? #f))
(check-false (pi-quota-limit? '()))

;; 2. 三个字段的访问器
(check (pi-quota-limit-kind limit) => "weekly")
(check (pi-quota-limit-code limit) => "1310")
(check (pi-quota-limit-message limit) => "每周额度已用尽")

;; 3. 官方消息缺失时为 #f
(check (pi-quota-limit-message (%make-pi-quota-limit "quota" "x" #f)) => #f)

(check-report)
