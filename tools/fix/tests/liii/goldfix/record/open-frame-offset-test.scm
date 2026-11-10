;; open-frame-offset
;; 读取 open-frame 左括号在源码中的偏移量。
;;
;; 语法
;; ----
;; (open-frame-offset frame)
;;
;; 参数
;; ----
;; frame : open-frame? open-frame 记录
;;
;; 返回值
;; ------
;; integer? 左括号在源码中的偏移量
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的偏移量
(check (open-frame-offset (make-open-frame :offset 9)) => 9)

;; 测试：缺省偏移量为 0
(check (open-frame-offset (make-open-frame)) => 0)

(check-report)
