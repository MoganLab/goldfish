(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-add-front!
;; 向可变长向量前端添加元素。
;;
;; 语法
;; ----
;; (flexvector-add-front! fv element ...)
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
(let ((fv (flexvector 'a)))
  (flexvector-add-front! fv 'b)
  (check (flexvector-ref fv 0) => 'b)
  (check (flexvector-ref fv 1) => 'a))

(check-report)
