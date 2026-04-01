(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; reverse-flexvector->list
;; 将可变长向量反向转换为列表。
;;
;; 语法
;; ----
;; (reverse-flexvector->list fv)
;; (reverse-flexvector->list fv start)
;; (reverse-flexvector->list fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
;; 源向量。
;;
;; start : exact-nonnegative-integer (可选)
;; 起始索引，默认为 0。
;;
;; end : exact-nonnegative-integer (可选)
;; 结束索引，默认为向量长度。
;;
;; 返回值
;; ----
;; list
;; 反向顺序的列表。
;;
;; 描述
;; ----
;; 将 flexvector 的元素以反向顺序转换为列表。

(let ((fv (flexvector 1 2 3)))
  (check (reverse-flexvector->list fv) => '(3 2 1))
) ;let

(let ((fv (flexvector 'a 'b 'c 'd)))
  (check (reverse-flexvector->list fv 1) => '(d c b))
  (check (reverse-flexvector->list fv 1 3) => '(c b))
) ;let

(check (reverse-flexvector->list (flexvector)) => '())

(check-report)
