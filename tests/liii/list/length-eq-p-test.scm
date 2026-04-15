(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; length=? 函数测试
;;
;; 语法
;; ----
;; (length=? n list)
;;
;; 参数
;; ----
;; n : integer?
;; 期望的长度，必须是非负整数。
;;
;; list : list?
;; 要检查的列表。
;;
;; 返回值
;; ------
;; boolean?
;; 如果列表长度等于n，返回#t，否则返回#f。
;;
;; 示例
;; ----
;; (length=? 3 (list 1 2 3)) => #t
;; (length=? 2 (list 1 2 3)) => #f
;; (length=? 4 (list 1 2 3)) => #f
;; (length=? 0 (list )) => #t


(check-true (length=? 3 (list 1 2 3)))
(check-false (length=? 2 (list 1 2 3)))
(check-false (length=? 4 (list 1 2 3)))


(check-true (length=? 0 (list)))
(check-catch 'value-error
  (length=? -1 (list))
) ;check-catch


(check-report)
