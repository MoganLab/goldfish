;; fix-token-text
;; 读取 token 对应的源码文本。
;;
;; 语法
;; ----
;; (fix-token-text token)
;;
;; 参数
;; ----
;; token : fix-token? token 记录
;;
;; 返回值
;; ------
;; string? token 对应的源码文本
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的源码文本
(check (fix-token-text (make-fix-token :text "(display")) => "(display")

;; 测试：缺省源码文本为空串
(check (fix-token-text (make-fix-token)) => "")

(check-report)
