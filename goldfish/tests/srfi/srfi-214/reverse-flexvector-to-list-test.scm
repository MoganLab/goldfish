;; reverse-flexvector->list
;; 可变长向量反向转换为列表。
;;
;; 语法
;; ----
;; (reverse-flexvector->list fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 待转换的可变长向量。
;;
;; 返回值
;; ----
;; list
;; 元素顺序与源相反的列表。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 1 2 3)))
  (check (reverse-flexvector->list fv) => '(3 2 1))
) ;let


(check-report)
