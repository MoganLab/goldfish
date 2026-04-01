(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-any
;; 检查是否存在满足条件的元素。
;;
;; 语法
;; ----
;; (flexvector-any pred? fv)
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
;; 如果存在满足条件的元素返回 #t，否则返回 #f。
;;
;; 描述
;; ----
;; 检查向量中是否存在至少一个元素满足谓词。

(let ((fv (flexvector 10 20 30)))
  (check (flexvector-any (lambda (x) (= x 20)) fv) => #t)
  (check (flexvector-any (lambda (x) (= x 21)) fv) => #f)
  (check (flexvector-any (lambda (x) (> x 25)) fv) => #t)
) ;let

(let ((fv (flexvector 1 3 5)))
  (check (flexvector-any even? fv) => #f)
  (check (flexvector-any odd? fv) => #t)
) ;let

(check-report)
