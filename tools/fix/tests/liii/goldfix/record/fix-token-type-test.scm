;; fix-token-type
;; 读取 token 的类型符号。
;;
;; 语法
;; ----
;; (fix-token-type token)
;;
;; 参数
;; ----
;; token : fix-token? token 记录
;;
;; 返回值
;; ------
;; symbol? token 类型，如 open-paren / close-paren / other
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的类型
(check (fix-token-type (make-fix-token :type 'open-paren)) => 'open-paren)
(check (fix-token-type (make-fix-token :type 'close-paren)) => 'close-paren)
(check (fix-token-type (make-fix-token :type 'line-comment)) => 'line-comment)

;; 测试：缺省类型为 other
(check (fix-token-type (make-fix-token)) => 'other)

(check-report)
