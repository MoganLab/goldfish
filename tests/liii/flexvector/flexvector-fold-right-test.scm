(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-fold-right
;; 对可变长向量进行右折叠。
;;
;; 语法
;; ----
;; (flexvector-fold-right proc nil fv)
;; (flexvector-fold-right proc nil fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : function
;; 折叠函数，形式为 (acc x) 或 (acc x1 x2 ...)。
;;
;; nil : any
;; 初始累加值。
;;
;; fv, fv1, fv2, ... : flexvector
;; 源向量。
;;
;; 返回值
;; ----
;; any
;; 最终的累加值。
;;
;; 描述
;; ----
;; 从右到左遍历向量，累积计算结果。

(let ((fv (flexvector 10 20 30)))
  (check (flexvector-fold-right (lambda (acc x) (cons x acc)) '() fv)
         => '(10 20 30)
  ) ;check
  (check (flexvector-fold-right + 0 fv) => 60)
) ;let

(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30)))
  (check (flexvector-fold-right (lambda (acc x y) (+ acc x y)) 0 fv1 fv2)
         => 66
  ) ;check
) ;let

(check-report)
