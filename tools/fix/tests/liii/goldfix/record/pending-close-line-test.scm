;; pending-close-line
;; 读取 pending-close 右括号所在的行号。
;;
;; 语法
;; ----
;; (pending-close-line pending)
;;
;; 参数
;; ----
;; pending : pending-close? pending-close 记录
;;
;; 返回值
;; ------
;; integer? 右括号所在的行号，从 1 开始
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的行号
(check (pending-close-line (make-pending-close :line 8)) => 8)

;; 测试：缺省行号为 1
(check (pending-close-line (make-pending-close)) => 1)

(check-report)
