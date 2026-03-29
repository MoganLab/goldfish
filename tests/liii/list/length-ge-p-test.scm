(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; length>=? 函数测试
;;
;; 语法
;; ----
;; (length>=? list n)
;;
;; 参数
;; ----
;; list : list?
;; 要检查的列表。
;;
;; n : integer?
;; 要比较的长度。
;;
;; 返回值
;; ------
;; boolean?
;; 如果列表长度大于或等于n，返回#t，否则返回#f。
;;
;; 示例
;; ----
;; (length>=? '(1 2 3 4 5) 3) => #t
;; (length>=? '(1 2) 3) => #f
;; (length>=? '() 0) => #t
;; (length>=? '(1) 0) => #t
;; (length>=? '(1 2 . 3) 3) => #f
;; (length>=? '(1 2 . 3) 2) => #t

(check-true (length>=? '(1 2 3 4 5) 3))
(check-false (length>=? '(1 2) 3))
(check-true (length>=? '() 0))

(check-true (length>=? '(1) 0))
(check-false (length>=? '() 1))

(check-false (length>=? '(1 2 . 3) 3))
(check-true (length>=? '(1 2 . 3) 2))

(check-report)
