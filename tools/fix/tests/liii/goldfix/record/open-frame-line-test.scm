;; open-frame-line
;; 读取 open-frame 左括号所在的行号。
;;
;; 语法
;; ----
;; (open-frame-line frame)
;;
;; 参数
;; ----
;; frame : open-frame? open-frame 记录
;;
;; 返回值
;; ------
;; integer? 左括号所在的行号，从 1 开始
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的行号
(check (open-frame-line (make-open-frame :line 4)) => 4)

;; 测试：缺省行号为 1
(check (open-frame-line (make-open-frame)) => 1)

(check-report)
