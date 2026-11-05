;; pending-close-end
;; 读取 pending-close 右括号在源码中的结束偏移量。
;;
;; 语法
;; ----
;; (pending-close-end pending)
;;
;; 参数
;; ----
;; pending : pending-close? pending-close 记录
;;
;; 返回值
;; ------
;; integer? 右括号在源码中的结束偏移量
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的结束偏移量
(check (pending-close-end (make-pending-close :end 17)) => 17)

;; 测试：缺省结束偏移量为 0
(check (pending-close-end (make-pending-close)) => 0)

(check-report)
