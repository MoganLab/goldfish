(import (liii check)
  (liii vector)
  (only (scheme base) let-values)
) ;import


(check-set-mode! 'report-failed)


;; vector-partition
;; 按谓词将向量重新排列，并返回命中数量。
;;
;; 语法
;; ----
;; (vector-partition pred vec)
;;
;; 参数
;; ----
;; pred : procedure?
;; 用于划分元素的谓词。
;;
;; vec : vector?
;; 要划分的向量。
;;
;; 返回值
;; ----
;; values
;; 返回两个值：
;; - 一个新的重排后向量
;; - 满足pred的元素个数
;;
;; 注意
;; ----
;; 满足pred的元素会被放到结果向量前部。
;;
;; 示例
;; ----
;; (vector-partition even? #(1 3 4 7 8)) => #(4 8 1 3 7), 2
;;
;; 错误处理
;; ----
;; wrong-type-arg 当pred不是过程，或vec不是向量时


(define (vector-partition->list pred v)
  (let-values (((ret cnt) (vector-partition pred v)))
    (list ret cnt)
  ) ;let-values
) ;define


(check (vector-partition->list even? #())
  =>
  '(#() 0)
) ;check
(check (vector-partition->list even?
         #(1 3 5 7 9)
       ) ;vector-partition->list
  =>
  '(#(1 3 5 7 9) 0)
) ;check
(check (vector-partition->list even?
         #(1 3 4 7 8)
       ) ;vector-partition->list
  =>
  '(#(4 8 1 3 7) 2)
) ;check


(check-report)
