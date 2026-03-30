(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; vector-index-right
;; 返回最后一个满足谓词的元素索引。
;;
;; 语法
;; ----
;; (vector-index-right pred vec)
;;
;; 参数
;; ----
;; pred : procedure?
;; 用于匹配元素的谓词。
;;
;; vec : vector?
;; 要搜索的向量。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 返回最后一个匹配元素的索引；如果没有匹配则返回#f。
;;
;; 注意
;; ----
;; 搜索方向为从右到左。
;;
;; 示例
;; ----
;; (vector-index-right even? #(1 3 4 6)) => 3
;; (vector-index-right even? #(1 3 5)) => #f
;;
;; 错误处理
;; ----
;; wrong-type-arg 当pred不是过程，或vec不是向量时

(check (vector-index-right even? #()) => #f)
(check (vector-index-right even? #(1 3 5 7 9)) => #f)
(check (vector-index-right even? #(1 3 4 7 8)) => 4)

(check-report)
