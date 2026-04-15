(import (liii check) (liii sort))


(check-set-mode! 'report-failed)


;; vector-merge
;; 合并两个已排序的向量，返回一个新的已排序向量。
;;
;; 语法
;; ----
;; (vector-merge cmp vec1 vec2)
;;
;; 参数
;; ----
;; cmp : procedure
;; 比较函数，接受两个参数，返回布尔值。
;;
;; vec1 : vector
;; 第一个已排序向量。
;;
;; vec2 : vector
;; 第二个已排序向量。
;;
;; 返回值
;; ----
;; vector
;; 合并后的新向量，包含两个输入向量的所有元素且保持有序。
;;
;; 注意
;; ----
;; 这是一个非破坏性操作。两个输入向量必须已经按相同顺序排序。
;;
;; 示例
;; ----
;; (vector-merge < #(1 3 5) #(2 4 6)) => #(1 2 3 4 5 6)
;;
;; 错误处理
;; ----
;; 无


;; 基本合并测试
(check (vector-merge < #(1 3 5) #(2 4 6))
  =>
  #(1 2 3 4 5 6)
) ;check
(check (vector-merge < #(1 1 3) #(1 2 4))
  =>
  #(1 1 1 2 3 4)
) ;check


;; 包含空向量的合并
(check (vector-merge < #() #(1 2 3))
  =>
  #(1 2 3)
) ;check
(check (vector-merge < #(1 2 3) #())
  =>
  #(1 2 3)
) ;check
(check (vector-merge < #() #()) => #())


;; 使用 pair 比较函数
(define (pair-< x y)
  (< (car x) (car y))
) ;define


(define (pair-full-< x y)
  (cond ((not (= (car x) (car y)))
         (< (car x) (car y))
        ) ;
        (else (< (cdr y) (cdr x)))
  ) ;cond
) ;define


(check-true (vector-sorted? pair-<
              (vector-merge pair-<
                #((1 . 1) (1 . 2) (3 . 1))
                #((1 . 3) (2 . 1) (3 . 2) (4 . 1))
              ) ;vector-merge
            ) ;vector-sorted?
) ;check-true
(check (vector-merge pair-<
         #((1 . 1) (1 . 2) (3 . 1))
         #((1 . 3) (2 . 1) (3 . 2) (4 . 1))
       ) ;vector-merge
  =>
  #((1 . 1) (1 . 2) (1 . 3) (2 . 1) (3 . 1) (3 . 2) (4 . 1))
) ;check


(check-true (vector-sorted? pair-full-<
              (vector-merge pair-full-<
                #((1 . 2) (1 . 1) (3 . 1))
                #((1 . 3) (2 . 1) (3 . 2) (4 . 1))
              ) ;vector-merge
            ) ;vector-sorted?
) ;check-true
(check (vector-merge pair-full-<
         #((1 . 2) (1 . 1) (3 . 1))
         #((1 . 3) (2 . 1) (3 . 2) (4 . 1))
       ) ;vector-merge
  =>
  #((1 . 3) (1 . 2) (1 . 1) (2 . 1) (3 . 2) (3 . 1) (4 . 1))
) ;check


(check-report)
