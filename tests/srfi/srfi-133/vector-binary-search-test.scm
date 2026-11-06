(import (liii check) (liii vector))

(check-set-mode! 'report-failed)

;; vector-binary-search
;; 在已排序向量中进行二分查找。
;;
;; 语法
;; ----
;; (vector-binary-search vec value cmp)
;; (vector-binary-search vec value cmp start)
;; (vector-binary-search vec value cmp start end)
;;
;; 参数
;; ----
;; vec : vector?
;; 已排序的向量，元素按 cmp 定义的顺序排列。
;;
;; value : any
;; 要查找的值。
;;
;; cmp : procedure?
;; 比较函数，接受两个参数 (value element)，返回负数表示 value < element，
;; 零表示相等，正数表示 value > element。
;;
;; start : integer? (可选)
;; 查找起始位置，默认为 0。
;;
;; end : integer? (可选)
;; 查找结束位置（不包含），默认为向量长度。
;;
;; 返回值
;; ----
;; exact nonnegative integer 或 #f
;; 返回匹配元素的索引；如果未找到则返回 #f。
;;
;; 注意
;; ----
;; 向量必须已按 cmp 定义的顺序排序，否则结果不确定。
;; 如果有多个匹配元素，可能返回其中任意一个的索引。
;;
;; 错误处理
;; ----
;; type-error 当 cmp 不是过程时

(define (cmp a b)
  (cond ((< a b) -1)
        ((= a b) 0)
        (else 1)
  ) ;cond
) ;define

(define v #(0 2 4 6 8 10 12))

(check (vector-binary-search v 0 cmp) => 0)
(check (vector-binary-search v 6 cmp) => 3)
(check (vector-binary-search v 12 cmp) => 6)
(check (vector-binary-search v 1 cmp) => #f)
(check (vector-binary-search v 13 cmp) => #f)
(check (vector-binary-search v -1 cmp) => #f)

;; with start
(check (vector-binary-search v 6 cmp 2) => 3)
(check (vector-binary-search v 0 cmp 2) => #f)

;; with start and end
(check (vector-binary-search v 6 cmp 2 5) => 3)
(check (vector-binary-search v 6 cmp 1 4) => 3)
(check (vector-binary-search v 8 cmp 1 4) => #f)

(check-report)
