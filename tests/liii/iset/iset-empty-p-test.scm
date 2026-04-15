(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-empty?
;; 检查集合是否为空。
;;
;; 语法
;; ----
;; (iset-empty? iset)
;;
;; 参数
;; ----
;; iset : iset
;; 要检查的集合。
;;
;; 返回值
;; -----
;; 如果 iset 为空，返回 #t；否则返回 #f。
;;
(check-true (iset-empty? (iset)))
(check-false (iset-empty? (iset 2 3 5 7 11))
) ;check-false
(check-false (iset-empty? (iset 1)))


(check-report)
