;; reverse-list->flexvector
;; 列表反向转换为可变长向量。
;;
;; 语法
;; ----
;; (reverse-list->flexvector list)
;;
;; 参数
;; ----
;; list : list
;; 待转换的列表。
;;
;; 返回值
;; ----
;; flexvector
;; 元素顺序与源列表相反的 flexvector。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(check (flexvector->list (reverse-list->flexvector '(1 2 3))) => '(3 2 1))

(check (flexvector->list (reverse-list->flexvector '())) => '())

;; 转换后仍是可变的 flexvector

(let ((fv (reverse-list->flexvector '(1 2 3))))
  (flexvector-add-back! fv 4)
  (check (flexvector->list fv) => '(3 2 1 4))
) ;let

(check-report)
