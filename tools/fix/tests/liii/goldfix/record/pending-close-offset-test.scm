;; pending-close-offset
;; 读取 pending-close 右括号在源码中的起始偏移量。
;;
;; 语法
;; ----
;; (pending-close-offset pending)
;;
;; 参数
;; ----
;; pending : pending-close? pending-close 记录
;;
;; 返回值
;; ------
;; integer? 右括号在源码中的起始偏移量
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的起始偏移量
(check (pending-close-offset (make-pending-close :offset 13 :end 14)) => 13)

;; 测试：缺省起始偏移量为 0
(check (pending-close-offset (make-pending-close)) => 0)

(check-report)
