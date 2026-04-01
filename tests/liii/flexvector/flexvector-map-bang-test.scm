(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-map!
;; 就地映射可变长向量。
;;
;; 语法
;; ----
;; (flexvector-map! proc fv)
;;
;; 参数
;; ----
;; proc : function
;; 映射函数。
;;
;; fv : flexvector
;; 目标向量。
;;
;; 返回值
;; ----
;; flexvector
;; 返回修改后的向量。
;;
;; 描述
;; ----
;; 对向量元素应用函数，就地修改原向量。

(let ((fv (flexvector 10 20 30)))
  (flexvector-map! (lambda (x) (* x 10)) fv)
  (check (flexvector->list fv) => '(100 200 300))
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-map! (lambda (x) (* x x)) fv)
  (check (flexvector->list fv) => '(1 4 9 16 25))
) ;let

(check-report)
