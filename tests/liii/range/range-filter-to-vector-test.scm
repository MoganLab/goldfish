(import (liii check) (liii range))


(check-set-mode! 'report-failed)


;; range-filter->vector
;; 过滤 range 中满足谓词的元素，结果为向量。
;;
;; 语法
;; ----
;; (range-filter->vector pred r)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数。
;;
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; vector
;; 向量，包含所有满足谓词的元素。
;;
;; 示例
;; ----
;; (range-filter->vector even? (numeric-range 0 10)) => #(0 2 4 6 8)
;;
;; 错误处理
;; ----
;; 无


(let ((r (numeric-range 0 10)))
  (check (range-filter->vector even? r)
    =>
    #(0 2 4 6 8)
  ) ;check
) ;let


(let ((r (numeric-range 0 10)))
  (check (range-filter->vector odd? r)
    =>
    #(1 3 5 7 9)
  ) ;check
) ;let


(let ((r (numeric-range 0 0)))
  (check (range-filter->vector even? r)
    =>
    #()
  ) ;check
) ;let


(let ((r (numeric-range 0 5)))
  (check (range-filter->vector (lambda (x) (> x 2))
           r
         ) ;range-filter->vector
    =>
    #(3 4)
  ) ;check
) ;let


(check-report)
