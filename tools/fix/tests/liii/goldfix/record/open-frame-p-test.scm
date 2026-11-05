;; open-frame?
;; 判断一个值是否为 open-frame 记录。
;;
;; 语法
;; ----
;; (open-frame? frame)
;;
;; 参数
;; ----
;; frame : any? 待判断的值
;;
;; 返回值
;; ------
;; boolean? 是 open-frame 记录时为 #t，否则为 #f
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：open-frame 记录返回 #t
(check (open-frame? (make-open-frame)) => #t)

;; 测试：其他值返回 #f
(check (open-frame? 'a) => #f)
(check (open-frame? #f) => #f)
(check (open-frame? 42) => #f)

(check-report)
