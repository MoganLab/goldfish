(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-front
;; 访问可变长向量中的第一个元素。
;;
;; 语法
;; ----
;; (flexvector-front fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; 返回值
;; -----
;; 返回第一个元素。
;;
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-front fv) => 'a))

(check-report)
