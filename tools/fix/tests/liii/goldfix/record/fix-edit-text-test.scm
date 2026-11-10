;; fix-edit-text
;; 读取 edit 插入的文本。
;;
;; 语法
;; ----
;; (fix-edit-text edit)
;;
;; 参数
;; ----
;; edit : fix-edit? edit 记录
;;
;; 返回值
;; ------
;; string? 插入的文本
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的插入文本
(check (fix-edit-text (make-fix-edit :text ")")) => ")")

;; 测试：缺省插入文本为空串
(check (fix-edit-text (make-fix-edit)) => "")

(check-report)
