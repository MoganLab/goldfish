(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset
;; 创建一个新的整数集合。
;;
;; 语法
;; ----
;; (iset element ...)
;;
;; 参数
;; ----
;; element ... : exact-integer
;; 初始元素（可选）。
;;
;; 返回值
;; -----
;; 返回包含指定元素的新 iset。
;;
(check-true (iset? (iset 1 2 3)))
(check (iset->list (iset 2 3 5 7 11))
  =>
  '(2 3 5 7 11)
) ;check
(check (iset->list (iset)) => '())


(check-report)
