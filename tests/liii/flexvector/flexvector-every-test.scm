(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-every
;; 检查是否全部元素满足条件。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-every pred? fv)
;; (flexvector-every pred? fv ...)
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
;; 如果全部元素满足 pred?，返回最后一个元素的判断结果（通常是 #t）。
;; 如果存在不满足的元素，返回 #f。
;; 空向量返回 #t（空真）。
;;
;; 另见
;; ----
;; flexvector-any - 检查是否存在满足


;; 基本测试
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-every (lambda (x) (< x 40))
           fv
         ) ;flexvector-every
    =>
    #t
  ) ;check
  (check (flexvector-every (lambda (x) (< x 30))
           fv
         ) ;flexvector-every
    =>
    #f
  ) ;check
) ;let


;; 全部满足
(let ((fv (flexvector 2 4 6 8)))
  (check (flexvector-every even? fv)
    =>
    #t
  ) ;check
) ;let


;; 有一个不满足
(let ((fv (flexvector 2 4 5 8)))
  (check (flexvector-every even? fv)
    =>
    #f
  ) ;check
) ;let


;; 第一个就不满足
(let ((fv (flexvector 1 2 4 6)))
  (check (flexvector-every even? fv)
    =>
    #f
  ) ;check
) ;let


;; 空向量返回 #t（空真）
(check (flexvector-every (lambda (x) #f)
         (flexvector)
       ) ;flexvector-every
  =>
  #t
) ;check


;; 单元素满足
(let ((fv (flexvector 42)))
  (check (flexvector-every (lambda (x) (= x 42))
           fv
         ) ;flexvector-every
    =>
    #t
  ) ;check
) ;let


;; 单元素不满足
(let ((fv (flexvector 42)))
  (check (flexvector-every (lambda (x) (= x 0))
           fv
         ) ;flexvector-every
    =>
    #f
  ) ;check
) ;let


;; 多向量版本
(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30))
     ) ;
  ;; 检查是否 fv1 所有元素都小于对应 fv2 元素
  (check (flexvector-every (lambda (x y) (< x y))
           fv1
           fv2
         ) ;flexvector-every
    =>
    #t
  ) ;check
) ;let


;; 多向量有一个不满足
(let ((fv1 (flexvector 1 2 100))
      (fv2 (flexvector 10 20 30))
     ) ;
  (check (flexvector-every (lambda (x y) (< x y))
           fv1
           fv2
         ) ;flexvector-every
    =>
    #f
  ) ;check
) ;let


;; 多向量长度不同取最短
(let ((fv1 (flexvector 1 2 3 4))
      (fv2 (flexvector 10 20))
     ) ;
  (check (flexvector-every (lambda (x y) (< x y))
           fv1
           fv2
         ) ;flexvector-every
    =>
    #t
  ) ;check
) ;let


(check-report)
