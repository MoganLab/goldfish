(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-partition
;; 将可变长向量分区为两个向量。
;;
;; 语法
;; ----
;; (flexvector-partition pred? fv)
;;
;; 参数
;; ----
;; pred? : function
;; 谓词函数。
;;
;; fv : flexvector
;; 源向量。
;;
;; 返回值
;; ----
;; values (flexvector flexvector)
;; 两个值：满足谓词的向量和不满足谓词的向量。
;;
;; 描述
;; ----
;; 将元素分为两组：满足谓词的和不满足谓词的。

(let ((fv (flexvector 10 20 30)))
  (let-values (((low high) (flexvector-partition (lambda (x) (< x 25)) fv)))
    (check (flexvector->vector low) => #(10 20))
    (check (flexvector->vector high) => #(30))
  ) ;let-values
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (let-values (((even odd) (flexvector-partition even? fv)))
    (check (flexvector->list even) => '(2 4))
    (check (flexvector->list odd) => '(1 3 5))
  ) ;let-values
) ;let

(check-report)
