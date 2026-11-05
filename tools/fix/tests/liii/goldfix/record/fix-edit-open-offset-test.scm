;; fix-edit-open-offset
;; 读取 edit 对应左括号的偏移量。
;;
;; 语法
;; ----
;; (fix-edit-open-offset edit)
;;
;; 参数
;; ----
;; edit : fix-edit? edit 记录
;;
;; 返回值
;; ------
;; integer? 对应左括号的偏移量，未知时为 #f
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的左括号偏移量
(check (fix-edit-open-offset (make-fix-edit :open-offset 6)) => 6)

;; 测试：缺省值为 #f
(check (fix-edit-open-offset (make-fix-edit)) => #f)

(check-report)
