(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset?
;; 检查对象是否为整数集合（iset）。
;;
;; 语法
;; ----
;; (iset? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要检查的对象。
;;
;; 返回值
;; -----
;; 如果 obj 是 iset，返回 #t；否则返回 #f。
;;
(check-true (iset? (iset)))
(check-true (iset? (iset 1 2 3)))
(check-false (iset? '()))
(check-false (iset? "not a set"))
(check-false (iset? 42))


(check-report)
