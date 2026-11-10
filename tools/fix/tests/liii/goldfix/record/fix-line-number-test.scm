;; fix-line-number
;; 读取行记录的行号。
;;
;; 语法
;; ----
;; (fix-line-number line)
;;
;; 参数
;; ----
;; line : fix-line? 行记录
;;
;; 返回值
;; ------
;; integer? 行号，从 1 开始
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的行号
(check (fix-line-number (make-fix-line :number 3)) => 3)

;; 测试：缺省行号为 1
(check (fix-line-number (make-fix-line)) => 1)

(check-report)
