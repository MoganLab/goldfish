(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-add!
;; 向可变长向量中添加元素。
;;
;; 语法
;; ----
;; (flexvector-add! fv index element ...)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; index : exact-nonnegative-integer
;; 插入位置。
;;
;; element ... : any
;; 要添加的元素。
;;
;; 返回值
;; -----
;; 返回修改后的 flexvector。
;;
(let ((fv (flexvector)))
  (flexvector-add! fv 0 'a)
  (check (flexvector-ref fv 0) => 'a))

(check-report)
