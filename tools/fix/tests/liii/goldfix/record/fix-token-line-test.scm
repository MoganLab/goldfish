;; fix-token-line
;; 读取 token 所在的行号。
;;
;; 语法
;; ----
;; (fix-token-line token)
;;
;; 参数
;; ----
;; token : fix-token? token 记录
;;
;; 返回值
;; ------
;; integer? token 所在的行号，从 1 开始
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的行号
(check (fix-token-line (make-fix-token :line 5)) => 5)

;; 测试：缺省行号为 1
(check (fix-token-line (make-fix-token)) => 1)

(check-report)
