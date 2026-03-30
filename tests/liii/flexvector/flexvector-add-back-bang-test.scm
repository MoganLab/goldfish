(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-add-back!
;; 向可变长向量后端添加元素。
;;
;; 语法
;; ----
;; (flexvector-add-back! fv element ...)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; element ... : any
;; 要添加的元素。
;;
;; 返回值
;; -----
;; 返回修改后的 flexvector。
;;
(let ((fv (flexvector)))
  (flexvector-add-back! fv 'a)
  (check (flexvector-length fv) => 1)
  (check (flexvector-ref fv 0) => 'a)
) ;let

(let ((fv (flexvector 'x 'y 'z)))
  (flexvector-add-back! fv 'w)
  (check (flexvector-length fv) => 4)
  (check (flexvector-ref fv 3) => 'w)
  (check (flexvector->list fv) => '(x y z w))
) ;let

(check-report)
