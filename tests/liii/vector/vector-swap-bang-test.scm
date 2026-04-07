(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; vector-swap!
;; 交换向量中两个索引位置的元素。
;;
;; 语法
;; ----
;; (vector-swap! vec i j)
;;
;; 参数
;; ----
;; vec : vector?
;; 要修改的向量。
;;
;; i : integer?
;; 第一个索引位置。
;;
;; j : integer?
;; 第二个索引位置。
;;
;; 返回值
;; ----
;; unspecified
;; 主要关注原向量被交换后的副作用。
;;
;; 注意
;; ----
;; 当i和j相同时，向量内容保持不变。
;;
;; 示例
;; ----
;; (vector-swap! #(0 1 2 3) 1 2) => 对应位置交换
;;
;; 错误处理
;; ----
;; out-of-range 当i或j超出向量边界时

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 1 2)
(check my-vector => #(0 2 1 3))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 1 1)
(check my-vector => #(0 1 2 3))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 0 (- (vector-length my-vector) 1))
(check my-vector => #(3 1 2 0))

(check-catch 'out-of-range
  (vector-swap! my-vector 1 (vector-length my-vector))
) ;check-catch

(check-report)
