(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-filter/index
;; 带索引过滤可变长向量。
;;
;; 语法
;; ----
;; (flexvector-filter/index pred? fv)
;;
;; 参数
;; ----
;; pred? : function
;; 接收索引和元素的谓词函数。
;;
;; fv : flexvector
;; 源向量。
;;
;; 返回值
;; ----
;; flexvector
;; 包含满足条件元素的新向量。
;;
;; 描述
;; ----
;; 返回包含所有满足谓词的元素的新 flexvector。
;; 谓词接收索引和元素两个参数。

(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector
           (flexvector-filter/index (lambda (i x) (not (= i 1))) fv))
         => #(10 30)
  ) ;check
) ;let

(let ((fv (flexvector 'a 'b 'c 'd 'e)))
  (check (flexvector->vector
           (flexvector-filter/index (lambda (i x) (even? i)) fv))
         => #(a c e)
  ) ;check
) ;let

(check-report)
