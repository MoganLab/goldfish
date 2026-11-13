;; bigint?
;; 判断对象是否为 bigint 记录类型。
;;
;; 语法
;; ----
;; (bigint? obj)
;;
;; 参数
;; ----
;; obj : any
;;   任意对象。
;;
;; 返回值
;; ----
;; boolean?
;;   如果 obj 是 bigint 则返回 #t，否则返回 #f。

(import (liii check)
        (liii bigint record)
) ;import

(check-set-mode! 'report-failed)

;; 正确的 bigint
(check-true (bigint? (make-bigint 0 '())))
(check-true (bigint? (make-bigint 1 '(123))))
(check-true (bigint? (make-bigint -1 '(456 789))))

;; 错误的类型
(check-false (bigint? 123))
(check-false (bigint? "hello"))
(check-false (bigint? '(1 2 3)))
(check-false (bigint? #t))
(check-false (bigint? '()))

;; 手动构造的不合法结构
(check-false (bigint? '(0)))
(check-false (bigint? '(1 2 3)))

(check-report)
