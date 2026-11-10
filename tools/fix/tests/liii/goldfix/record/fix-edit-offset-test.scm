;; fix-edit-offset
;; 读取 edit 的插入位置偏移量。
;;
;; 语法
;; ----
;; (fix-edit-offset edit)
;;
;; 参数
;; ----
;; edit : fix-edit? edit 记录
;;
;; 返回值
;; ------
;; integer? 插入位置偏移量
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的插入位置
(check (fix-edit-offset (make-fix-edit :offset 8)) => 8)

;; 测试：缺省插入位置为 0
(check (fix-edit-offset (make-fix-edit)) => 0)

(check-report)
