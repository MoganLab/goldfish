;; fix-token-end
;; 读取 token 在源码中的结束偏移量。
;;
;; 语法
;; ----
;; (fix-token-end token)
;;
;; 参数
;; ----
;; token : fix-token? token 记录
;;
;; 返回值
;; ------
;; integer? token 的结束偏移量
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的结束偏移量
(check (fix-token-end (make-fix-token :offset 3 :end 9)) => 9)

;; 测试：缺省结束偏移量为 0
(check (fix-token-end (make-fix-token)) => 0)

(check-report)
