(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; null? 函数测试
;;
;; 语法
;; ----
;; (null? obj)
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
;; (null? '()) => #t
;; (null? 1) => #f

(check (null? 1) => #f)

(check-report)
