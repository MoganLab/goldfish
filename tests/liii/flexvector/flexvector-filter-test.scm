(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-filter
;; 过滤满足条件的元素，返回新向量。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-filter pred? fv)
;; (flexvector-filter pred? fv ...)
;;
;; 参数
;; ----
;; pred? : procedure
;;   谓词函数，返回布尔值。
;;
;; fv : flexvector
;;   源向量。
;;
;; 返回值
;; -----
;; 返回新的 flexvector，只包含满足 pred? 的元素。
;; 原向量不被修改。
;;
;; 另见
;; ----
;; flexvector-filter! - 原地过滤
;; flexvector-filter/index - 带索引过滤
;; flexvector-partition - 分区为两部分

;; 基本过滤
(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector
           (flexvector-filter (lambda (x) (< x 25)) fv))
         => #(10 20)
  ) ;check
  ;; 原向量不变
  (check (flexvector-length fv) => 3)
) ;let

;; 过滤出偶数
(let ((fv (flexvector 1 2 3 4 5 6)))
  (check (flexvector->list
           (flexvector-filter even? fv))
         => '(2 4 6)
  ) ;check
) ;let

;; 全部满足
(let ((fv (flexvector 2 4 6)))
  (check (flexvector->vector
           (flexvector-filter even? fv))
         => #(2 4 6)
  ) ;check
) ;let

;; 全部不满足
(let ((fv (flexvector 1 3 5)))
  (check (flexvector->vector
           (flexvector-filter even? fv))
         => #()
  ) ;check
) ;let

;; 空向量
(check (flexvector->vector
         (flexvector-filter (lambda (x) #t) (flexvector)))
       => #()
) ;check

;; 单元素满足
(let ((fv (flexvector 42)))
  (check (flexvector->vector
           (flexvector-filter (lambda (x) (= x 42)) fv))
         => #(42)
  ) ;check
) ;let

;; 单元素不满足
(let ((fv (flexvector 42)))
  (check (flexvector->vector
           (flexvector-filter (lambda (x) (= x 0)) fv))
         => #()
  ) ;check
) ;let

;; 多向量过滤
(let ((fv1 (flexvector 1 2 3 4))
      (fv2 (flexvector 10 20 30 40)))
  (check (flexvector->vector
           (flexvector-filter (lambda (x y) (< (+ x y) 35)) fv1 fv2))
         => #(1 2 3)  ; 1+10=11<35, 2+20=22<35, 3+30=33<35, 4+40=44>=35
  ) ;check
) ;let

(check-report)
