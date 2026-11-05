;; fix-edit?
;; 判断一个值是否为 fix-edit 记录。
;;
;; 语法
;; ----
;; (fix-edit? edit)
;;
;; 参数
;; ----
;; edit : any? 待判断的值
;;
;; 返回值
;; ------
;; boolean? 是 fix-edit 记录时为 #t，否则为 #f
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：fix-edit 记录返回 #t
(check (fix-edit? (make-fix-edit)) => #t)

;; 测试：其他值返回 #f
(check (fix-edit? 'a) => #f)
(check (fix-edit? #f) => #f)
(check (fix-edit? 42) => #f)

(check-report)
