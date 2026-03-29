(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; null-list? 函数测试
;;
;; 语法
;; ----
;; (null-list? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象
;;
;; 返回值
;; ----
;; boolean?
;; 当 obj 是空列表时返回 #t，否则返回 #f
;;
;; 示例
;; ----
;; (null-list? '()) => #t
;; (null-list? '(1 . 2)) => #f
;; (null-list? '(1 2)) => #f

(check (null-list? '()) => #t)
(check (null-list? '(1 . 2)) => #f)
(check (null-list? '(1 2)) => #f)

(check-report)
