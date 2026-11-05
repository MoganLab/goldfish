;; fix-edit-end
;; 读取 edit 删除区间的结束偏移量。
;;
;; 语法
;; ----
;; (fix-edit-end edit)
;;
;; 参数
;; ----
;; edit : fix-edit? edit 记录
;;
;; 返回值
;; ------
;; integer? 删除区间的结束偏移量
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的结束偏移量
(check (fix-edit-end (make-fix-edit :kind 'delete :start 3 :end 9)) => 9)

;; 测试：缺省结束偏移量为 0
(check (fix-edit-end (make-fix-edit)) => 0)

(check-report)
