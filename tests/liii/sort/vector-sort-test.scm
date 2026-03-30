(import (liii check)
        (liii sort)
) ;import

(check-set-mode! 'report-failed)

;; vector-sort
;; 对向量进行非破坏性快速排序。
;;
;; 语法
;; ----
;; (vector-sort cmp vec)
;;
;; 参数
;; ----
;; cmp : procedure
;; 比较函数，接受两个参数，返回布尔值。
;;
;; vec : vector
;; 要排序的向量。
;;
;; 返回值
;; ----
;; vector
;; 排序后的新向量，原向量保持不变。
;;
;; 注意
;; ----
;; 这是一个非破坏性操作，原向量不会被修改。
;;
;; 示例
;; ----
;; (vector-sort < #(3 1 4 1 5 9 2 6)) => #(1 1 2 3 4 5 6 9)
;;
;; 错误处理
;; ----
;; 无

(check-true (vector-sorted? < (vector-sort < #(1 5 1 0 -1 9 2 4 3))))
(check (vector-sort < #(3 1 4 1 5 9 2 6 5)) => #(1 1 2 3 4 5 5 6 9))

;; 边界情况
(check (vector-sort < #()) => #())
(check (vector-sort < #(42)) => #(42))
(check (vector-sort < #(1 2 3 4 5)) => #(1 2 3 4 5))
(check (vector-sort > #(1 2 3 4 5)) => #(5 4 3 2 1))

;; 确保原向量未被修改
(define test-vec #(3 1 4 1 5 9 2 6 5))
(define sorted-vec (vector-sort < test-vec))
(check (equal? test-vec #(3 1 4 1 5 9 2 6 5)) => #t)

(check-report)
