;; pi-quota-limit?
;; 判断对象是否为额度耗尽判定结果 record 实例。
;;
;; 语法
;; ----
;; (pi-quota-limit? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于额度耗尽判定结果 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-quota-limit "weekly" "1310" "每周额度已用尽")))
  (check-true (pi-quota-limit? obj))
) ;let
(check-false (pi-quota-limit? '()))
(check-false (pi-quota-limit? #f))
(check-false (pi-quota-limit? "not-a-record"))

(check-report)
