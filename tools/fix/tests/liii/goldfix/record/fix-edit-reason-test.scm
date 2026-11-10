;; fix-edit-reason
;; 读取 edit 的修改原因。
;;
;; 语法
;; ----
;; (fix-edit-reason edit)
;;
;; 参数
;; ----
;; edit : fix-edit? edit 记录
;;
;; 返回值
;; ------
;; string? 修改原因，如 eof / extra-close
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的修改原因
(check (fix-edit-reason (make-fix-edit :reason "eof")) => "eof")
(check (fix-edit-reason (make-fix-edit :reason "extra-close")) => "extra-close")

;; 测试：缺省修改原因为空串
(check (fix-edit-reason (make-fix-edit)) => "")

(check-report)
