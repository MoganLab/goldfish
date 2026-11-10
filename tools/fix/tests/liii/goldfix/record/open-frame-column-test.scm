;; open-frame-column
;; 读取 open-frame 左括号所在的列号。
;;
;; 语法
;; ----
;; (open-frame-column frame)
;;
;; 参数
;; ----
;; frame : open-frame? open-frame 记录
;;
;; 返回值
;; ------
;; integer? 左括号所在的列号，从 0 开始
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的列号
(check (open-frame-column (make-open-frame :column 6)) => 6)

;; 测试：缺省列号为 0
(check (open-frame-column (make-open-frame)) => 0)

(check-report)
