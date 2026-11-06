;;
;; iset=?
;; 检查两个或多个集合是否相等（包含相同元素）。
;;

;; 语法
;; ----
;; (iset=? iset1 iset2 ...)
;;

;; 参数
;; ----
;; set1 : iset
;; 第一个集合。
;;
;; set2 : iset
;; 第二个集合。

;; 返回值
;; ----
;; boolean
;; 两个集合包含相同元素时返回 #t，否则返回 #f。

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(check-true (iset=? (iset) (iset)))
(check-true (iset=? (iset 1 2 3 4) (iset 2 1 4 3)))
(check-true (iset=? (iset 1 2 3 4) (iset 2 1 4 3) (iset 3 2 1 4)))
(check-false (iset=? (iset 1 2 3 4) (iset 2 3 4)))


(check-report)
