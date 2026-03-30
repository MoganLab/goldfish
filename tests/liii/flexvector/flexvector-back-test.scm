(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-back
;; 访问可变长向量中的最后一个元素。
;;
;; 语法
;; ----
;; (flexvector-back fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; 返回值
;; -----
;; 返回最后一个元素。
;;
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-back fv) => 'c))

(check-report)
