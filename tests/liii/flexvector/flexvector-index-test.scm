(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-index
;; 查找第一个满足条件的元素索引。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-index pred? fv)
;; (flexvector-index pred? fv ...)
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
;; 返回第一个满足 pred? 的元素的索引，从 0 开始。
;; 如果没有元素满足条件，返回 #f。
;;
;; 另见
;; ----
;; flexvector-index-right - 从右侧查找
;; flexvector-skip - 跳过满足条件的元素
;; flexvector-binary-search - 二分查找（有序向量）


;; 基本查找
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-index (lambda (x) (> x 10))
           fv
         ) ;flexvector-index
    =>
    1
  ) ;check
) ;let


;; 查找第一个满足条件的
(let ((fv (flexvector 1 3 5 4 2)))
  (check (flexvector-index even? fv) => 3)
) ;let


;; 没找到返回 #f
(let ((fv (flexvector 1 3 5)))
  (check (flexvector-index even? fv)
    =>
    #f
  ) ;check
) ;let


;; 空向量
(check (flexvector-index (lambda (x) #t)
         (flexvector)
       ) ;flexvector-index
  =>
  #f
) ;check


;; 第一个元素就满足
(let ((fv (flexvector 2 4 6)))
  (check (flexvector-index even? fv) => 0)
) ;let


;; 最后一个元素满足
(let ((fv (flexvector 1 3 5 7 8)))
  (check (flexvector-index even? fv) => 4)
) ;let


;; 单元素满足
(let ((fv (flexvector 42)))
  (check (flexvector-index (lambda (x) (= x 42))
           fv
         ) ;flexvector-index
    =>
    0
  ) ;check
) ;let


;; 单元素不满足
(let ((fv (flexvector 42)))
  (check (flexvector-index (lambda (x) (= x 0))
           fv
         ) ;flexvector-index
    =>
    #f
  ) ;check
) ;let


;; 多向量查找
(let ((fv1 (flexvector 1 2 3 4))
      (fv2 (flexvector 10 5 3 1))
     ) ;
  (check (flexvector-index (lambda (x y) (> x y))
           fv1
           fv2
         ) ;flexvector-index
    =>
    3
  ) ;check
) ;let


;; 多向量没满足
(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30))
     ) ;
  (check (flexvector-index (lambda (x y) (> x y))
           fv1
           fv2
         ) ;flexvector-index
    =>
    #f
  ) ;check
) ;let


(check-report)
