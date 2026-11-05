;; open-frame-tag-name
;; 读取 open-frame 左括号对应的标记名称。
;;
;; 语法
;; ----
;; (open-frame-tag-name frame)
;;
;; 参数
;; ----
;; frame : open-frame? open-frame 记录
;;
;; 返回值
;; ------
;; string? 左括号对应的标记名称，未知时为空串
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的标记名称
(check (open-frame-tag-name (make-open-frame :tag-name "define")) => "define")

;; 测试：缺省标记名称为空串
(check (open-frame-tag-name (make-open-frame)) => "")

(check-report)
