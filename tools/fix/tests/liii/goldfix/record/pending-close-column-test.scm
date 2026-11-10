;; pending-close-column
;; 读取 pending-close 右括号所在的列号。
;;
;; 语法
;; ----
;; (pending-close-column pending)
;;
;; 参数
;; ----
;; pending : pending-close? pending-close 记录
;;
;; 返回值
;; ------
;; integer? 右括号所在的列号，从 0 开始
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的列号
(check (pending-close-column (make-pending-close :column 11)) => 11)

;; 测试：缺省列号为 0
(check (pending-close-column (make-pending-close)) => 0)

(check-report)
