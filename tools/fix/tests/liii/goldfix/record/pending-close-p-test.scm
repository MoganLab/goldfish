;; pending-close?
;; 判断一个值是否为 pending-close 记录。
;;
;; 语法
;; ----
;; (pending-close? pending)
;;
;; 参数
;; ----
;; pending : any? 待判断的值
;;
;; 返回值
;; ------
;; boolean? 是 pending-close 记录时为 #t，否则为 #f
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：pending-close 记录返回 #t
(check (pending-close? (make-pending-close)) => #t)

;; 测试：其他值返回 #f
(check (pending-close? 'a) => #f)
(check (pending-close? #f) => #f)
(check (pending-close? 42) => #f)

(check-report)
