;; fix-edit-start
;; 读取 edit 删除区间的起始偏移量。
;;
;; 语法
;; ----
;; (fix-edit-start edit)
;;
;; 参数
;; ----
;; edit : fix-edit? edit 记录
;;
;; 返回值
;; ------
;; integer? 删除区间的起始偏移量
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的起始偏移量
(check (fix-edit-start (make-fix-edit :kind 'delete :start 3 :end 9)) => 3)

;; 测试：缺省起始偏移量为 0
(check (fix-edit-start (make-fix-edit)) => 0)

(check-report)
