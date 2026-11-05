;; fix-token-offset
;; 读取 token 在源码中的起始偏移量。
;;
;; 语法
;; ----
;; (fix-token-offset token)
;;
;; 参数
;; ----
;; token : fix-token? token 记录
;;
;; 返回值
;; ------
;; integer? token 的起始偏移量
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的起始偏移量
(check (fix-token-offset (make-fix-token :offset 7 :end 8)) => 7)

;; 测试：缺省起始偏移量为 0
(check (fix-token-offset (make-fix-token)) => 0)

(check-report)
