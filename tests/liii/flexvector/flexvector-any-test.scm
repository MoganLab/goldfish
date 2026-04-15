(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-any
;; 检查是否存在满足条件的元素。找到后立即返回，不完全遍历。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-any pred? fv)
;; (flexvector-any pred? fv ...)
;;
;; 参数
;; ----
;; pred? : procedure
;;   谓词函数。
;;
;; fv : flexvector
;;   源向量。
;;
;; 返回值
;; -----
;; 如果存在满足 pred? 的元素，返回该元素。
;; 如果没有元素满足条件，返回 #f。
;;
;; 注意：如果 #f 是可能的有效值，需要注意与"未找到"区分。
;;
;; 另见
;; ----
;; flexvector-every - 检查是否全部满足
;; flexvector-index - 查找位置


;; 基本测试：找到返回元素
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-any (lambda (x) (= x 20))
           fv
         ) ;flexvector-any
    =>
    #t
  ) ;check
  (check (flexvector-any (lambda (x) (= x 21))
           fv
         ) ;flexvector-any
    =>
    #f
  ) ;check
) ;let


;; 第一个就满足
(let ((fv (flexvector 2 4 6 8)))
  (check (flexvector-any even? fv) => #t)
) ;let


;; 最后一个满足
(let ((fv (flexvector 1 3 5 7 8)))
  (check (flexvector-any even? fv) => #t)
) ;let


;; 都不满足
(let ((fv (flexvector 1 3 5)))
  (check (flexvector-any even? fv) => #f)
) ;let


;; 空向量
(check (flexvector-any (lambda (x) #t)
         (flexvector)
       ) ;flexvector-any
  =>
  #f
) ;check


;; 单元素满足
(let ((fv (flexvector 42)))
  (check (flexvector-any (lambda (x) (= x 42))
           fv
         ) ;flexvector-any
    =>
    #t
  ) ;check
) ;let


;; 单元素不满足
(let ((fv (flexvector 42)))
  (check (flexvector-any (lambda (x) (= x 0)) fv)
    =>
    #f
  ) ;check
) ;let


;; 多向量版本
(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 3 2 5))
     ) ;
  ;; 检查是否有对应位置元素相等
  (check (flexvector-any (lambda (x y) (= x y))
           fv1
           fv2
         ) ;flexvector-any
    =>
    #t
  ) ;check
) ;let


;; 多向量都不满足
(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 4 5 6))
     ) ;
  (check (flexvector-any (lambda (x y) (= x y))
           fv1
           fv2
         ) ;flexvector-any
    =>
    #f
  ) ;check
) ;let


(check-report)
