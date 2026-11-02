;; pi-quota-limit?
;; 额度耗尽判定结果的 record 谓词。
;;
;; 语法
;; ----
;; (pi-quota-limit? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象，通常来自 provider 的额度识别函数。
;;
;; 返回值
;; ----
;; boolean
;; 若属于 pi-quota-limit record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

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
