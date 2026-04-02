(import (liii check)
        (rename (srfi srfi-78)
                (check-reset! srfi-78-check-reset!)
        ) ;rename
) ;import

(check-set-mode! 'summary)

;; check-failed?
;; 判断当前测试上下文中是否已经存在失败断言。
;;
;; 语法
;; ----
;; (check-failed?)

(check-false (check-failed?))

(check:proc 'check-failed-probe
            (lambda () 1)
            2)

(check-true (check-failed?))

(srfi-78-check-reset!)

(check-false (check-failed?))

(check-report)
