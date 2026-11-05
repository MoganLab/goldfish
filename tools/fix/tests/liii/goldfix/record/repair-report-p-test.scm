;; repair-report?
;; 判断一个值是否为 repair-report 记录。
;;
;; 语法
;; ----
;; (repair-report? report)
;;
;; 参数
;; ----
;; report : any? 待判断的值
;;
;; 返回值
;; ------
;; boolean? 是 repair-report 记录时为 #t，否则为 #f
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：repair-report 记录返回 #t
(check (repair-report? (make-repair-report)) => #t)

;; 测试：其他值返回 #f
(check (repair-report? 'a) => #f)
(check (repair-report? #f) => #f)
(check (repair-report? 42) => #f)

(check-report)
