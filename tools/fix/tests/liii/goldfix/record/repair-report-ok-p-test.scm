;; repair-report-ok?
;; 读取 report 中修复后括号是否平衡的标志。
;;
;; 语法
;; ----
;; (repair-report-ok? report)
;;
;; 参数
;; ----
;; report : repair-report? report 记录
;;
;; 返回值
;; ------
;; boolean? 括号平衡时为 #t，否则为 #f
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的标志
(check (repair-report-ok? (make-repair-report :ok? #t)) => #t)
(check (repair-report-ok? (make-repair-report :ok? #f)) => #f)

;; 测试：缺省值为 #f
(check (repair-report-ok? (make-repair-report)) => #f)

(check-report)
