(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-add-back!
;; 在可变长向量尾部添加元素。
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
;; ----
;; flexvector
;; 返回修改后的 flexvector。
;;
;; 描述
;; ----
;; 在向量末尾追加一个或多个元素，这是最常用的添加操作。

(let ((fv (flexvector)))
  (flexvector-add-back! fv 'a)
  (check (flexvector-length fv) => 1)
  (check (flexvector-ref fv 0) => 'a)
) ;let

(let ((fv (flexvector 1 2)))
  (flexvector-add-back! fv 3 4)
  (check (flexvector->list fv) => '(1 2 3 4))
) ;let

(check-report)
