(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-add-front!
;; 在可变长向量头部添加元素。
;;
;; 语法
;; ----
;; (flexvector-add-front! fv element)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
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
;; 在向量开头插入元素。

(let ((fv (flexvector 'a 'b)))
  (flexvector-add-front! fv 'x)
  (check (flexvector-ref fv 0) => 'x)
  (check (flexvector-ref fv 1) => 'a)
) ;let

(let ((fv (flexvector 'a)))
  (flexvector-add-front! fv 'x)
  (check (flexvector-length fv) => 2)
  (check (flexvector-ref fv 0) => 'x)
  (check (flexvector-ref fv 1) => 'a)
) ;let

(check-report)
