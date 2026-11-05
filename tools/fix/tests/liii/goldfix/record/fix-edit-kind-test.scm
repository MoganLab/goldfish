;; fix-edit-kind
;; 读取 edit 的类型符号。
;;
;; 语法
;; ----
;; (fix-edit-kind edit)
;;
;; 参数
;; ----
;; edit : fix-edit? edit 记录
;;
;; 返回值
;; ------
;; symbol? insert 或 delete
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的类型
(check (fix-edit-kind (make-fix-edit :kind 'insert)) => 'insert)
(check (fix-edit-kind (make-fix-edit :kind 'delete)) => 'delete)

;; 测试：缺省类型为 insert
(check (fix-edit-kind (make-fix-edit)) => 'insert)

(check-report)
