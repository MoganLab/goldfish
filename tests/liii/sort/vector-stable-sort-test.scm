(import (liii check)
        (liii sort)
) ;import

(check-set-mode! 'report-failed)

;; vector-stable-sort
;; 对向量进行非破坏性稳定排序。
;;
;; 语法
;; ----
;; (vector-stable-sort cmp vec)
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
;; 排序后的新向量，原向量保持不变。相等元素的相对顺序保持不变。
;;
;; 注意
;; ----
;; 这是一个非破坏性操作，原向量不会被修改。稳定排序保证相等元素的原始顺序。
;;
;; 示例
;; ----
;; (vector-stable-sort < #(1 5 1 0 -1 9 2 4 3)) => 稳定排序后的向量
;;
;; 错误处理
;; ----
;; 无

(check-true (vector-sorted? < (vector-stable-sort < #(1 5 1 0 -1 9 2 4 3))))
(check-true (vector-sorted? < (vector-stable-sort < #(9 7 5 3 2 8 6 4 1))))

;; 边界情况
(check (vector-stable-sort < #()) => #())
(check (vector-stable-sort < #(42)) => #(42))
(check (vector-stable-sort < #(1 2 3 4 5)) => #(1 2 3 4 5))

;; 确保原向量未被修改
(define test-vec #(3 1 4 1 5 9 2 6 5))
(define sorted-vec (vector-stable-sort < test-vec))
(check (equal? test-vec #(3 1 4 1 5 9 2 6 5)) => #t)

(check-report)
