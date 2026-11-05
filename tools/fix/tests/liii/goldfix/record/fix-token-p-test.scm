;; fix-token?
;; 判断一个值是否为 fix-token 记录。
;;
;; 语法
;; ----
;; (fix-token? token)
;;
;; 参数
;; ----
;; token : any? 待判断的值
;;
;; 返回值
;; ------
;; boolean? 是 fix-token 记录时为 #t，否则为 #f
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：fix-token 记录返回 #t
(check (fix-token? (make-fix-token)) => #t)

;; 测试：其他值返回 #f
(check (fix-token? 'a) => #f)
(check (fix-token? #f) => #f)
(check (fix-token? 42) => #f)

(check-report)
