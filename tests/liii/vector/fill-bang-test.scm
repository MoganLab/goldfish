(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; fill!
;; 向量填充别名测试。
;;
;; 语法
;; ----
;; (fill! vec fill)
;; (fill! vec fill start end)
;;
;; 参数
;; ----
;; vec : vector?
;; 要填充的向量。
;;
;; fill : any?
;; 要写入的值。
;;
;; start : integer? 可选
;; 起始位置（包含）。
;;
;; end : integer? 可选
;; 结束位置（不包含）。
;;
;; 返回值
;; ----
;; unspecified
;; 主要关注原向量被填充后的副作用。
;;
;; 注意
;; ----
;; fill! 在这里作为vector-fill!的便捷别名使用。
;;
;; 示例
;; ----
;; (fill! #(0 1 2) #f) => 所有元素变为#f
;;
;; 错误处理
;; ----
;; 继承vector-fill!的错误处理行为

(define my-vector (vector 0 1 2 3 4))
(fill! my-vector #f)
(check my-vector => #(#f #f #f #f #f))

(define my-vector (vector 0 1 2 3 4))
(fill! my-vector #f 1 2)
(check my-vector => #(0 #f 2 3 4))

(check-report)
