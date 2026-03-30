(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-partition
;; 分区操作。
;;
;; 语法
;; ----
;; (flexvector-partition pred? fv)
;;
;; 返回值
;; -----
;; 返回两个值：满足谓词的 flexvector 和不满足谓词的 flexvector。
;;
(let ((fv (flexvector 10 20 30)))
  (let-values (((low high) (flexvector-partition (lambda (x) (< x 25)) fv)))
    (check (flexvector->vector low) => #(10 20))
    (check (flexvector->vector high) => #(30))))

(check-report)
