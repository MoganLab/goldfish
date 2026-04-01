(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-every
;; 检查是否所有元素都满足条件。
;;
;; 语法
;; ----
;; (flexvector-every pred? fv)
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
;; boolean
;; 如果所有元素都满足条件返回 #t，否则返回 #f。
;;
;; 描述
;; ----
;; 检查向量中所有元素是否都满足谓词。

(let ((fv (flexvector 10 20 30)))
  (check (flexvector-every (lambda (x) (< x 40)) fv) => #t)
  (check (flexvector-every (lambda (x) (< x 30)) fv) => #f)
  (check (flexvector-every (lambda (x) (>= x 10)) fv) => #t)
) ;let

(let ((fv (flexvector 2 4 6)))
  (check (flexvector-every even? fv) => #t)
  (check (flexvector-every odd? fv) => #f)
) ;let

(check-report)
