;; flexvector-reverse-copy
;; 反向复制可变长向量。
;;
;; 语法
;; ----
;; (flexvector-reverse-copy fv)
;; (flexvector-reverse-copy fv start)
;; (flexvector-reverse-copy fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
;; 源可变长向量。
;;
;; start : exact-integer
;; 复制起始索引（可选）。
;;
;; end : exact-integer
;; 复制结束索引，不含（可选）。
;;
;; 返回值
;; ----
;; flexvector
;; 元素顺序与源区间相反的新 flexvector。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 1 2 3)))
  (let ((rev (flexvector-reverse-copy fv)))
    (check (flexvector->list rev) => '(3 2 1))
    (check (flexvector->list fv) => '(1 2 3))
  ) ;let
) ;let


(check-report)
