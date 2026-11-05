;; fix-line?
;; 判断一个值是否为 fix-line 记录。
;;
;; 语法
;; ----
;; (fix-line? line)
;;
;; 参数
;; ----
;; line : any? 待判断的值
;;
;; 返回值
;; ------
;; boolean? 是 fix-line 记录时为 #t，否则为 #f
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：fix-line 记录返回 #t
(check (fix-line? (make-fix-line)) => #t)

;; 测试：其他值返回 #f
(check (fix-line? 'a) => #f)
(check (fix-line? #f) => #f)
(check (fix-line? 42) => #f)

(check-report)
