(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-add!
;; 在指定位置向可变长向量中添加元素。
;;
;; 语法
;; ----
;; (flexvector-add! fv index element)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; index : exact-nonnegative-integer
;; 插入位置。
;;
;; element : any
;; 要添加的元素。
;;
;; 返回值
;; ----
;; flexvector
;; 返回修改后的 flexvector。
;;
;; 描述
;; ----
;; 在指定索引位置插入元素，原有元素向后移动。

(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-add! fv 1 'x)
  (check (flexvector-ref fv 0) => 'a)
  (check (flexvector-ref fv 1) => 'x)
  (check (flexvector-ref fv 2) => 'b)
) ;let

(let ((fv (flexvector 'b 'a)))
  (flexvector-add! fv 1 'c)
  (check (flexvector-ref fv 0) => 'b)
  (check (flexvector-ref fv 1) => 'c)
  (check (flexvector-ref fv 2) => 'a)
) ;let

(check-report)
