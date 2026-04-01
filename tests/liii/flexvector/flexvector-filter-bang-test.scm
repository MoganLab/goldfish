(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-filter!
;; 就地过滤可变长向量。
;;
;; 语法
;; ----
;; (flexvector-filter! pred? fv)
;;
;; 参数
;; ----
;; pred? : function
;; 谓词函数。
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
;; 移除所有不满足谓词的元素，就地修改原向量。

(let ((fv (flexvector 10 20 30)))
  (flexvector-filter! (lambda (x) (< x 25)) fv)
  (check (flexvector->list fv) => '(10 20))
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-filter! odd? fv)
  (check (flexvector->list fv) => '(1 3 5))
) ;let

(check-report)
