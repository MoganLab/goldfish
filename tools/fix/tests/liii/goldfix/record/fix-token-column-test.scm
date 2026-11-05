;; fix-token-column
;; 读取 token 所在的列号。
;;
;; 语法
;; ----
;; (fix-token-column token)
;;
;; 参数
;; ----
;; token : fix-token? token 记录
;;
;; 返回值
;; ------
;; integer? token 所在的列号，从 0 开始
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的列号
(check (fix-token-column (make-fix-token :column 4)) => 4)

;; 测试：缺省列号为 0
(check (fix-token-column (make-fix-token)) => 0)

(check-report)
