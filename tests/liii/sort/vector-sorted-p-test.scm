(import (liii check) (liii sort))


(check-set-mode! 'report-failed)


;; vector-sorted?
;; 检查向量是否已按指定比较函数排序。
;;
;; 语法
;; ----
;; (vector-sorted? cmp vec)
;; (vector-sorted? cmp vec start)
;; (vector-sorted? cmp vec start end)
;;
;; 参数
;; ----
;; cmp : procedure
;; 比较函数，接受两个参数，返回布尔值。
;;
;; vec : vector
;; 要检查的向量。
;;
;; start : integer (可选)
;; 起始索引（包含）。
;;
;; end : integer (可选)
;; 结束索引（不包含）。
;;
;; 返回值
;; ----
;; boolean
;; 如果向量已排序返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (vector-sorted? < #(1 2 3 4 5)) => #t
;; (vector-sorted? < #(5 1 2 3 4) 1) => #t
;;
;; 错误处理
;; ----
;; Invalid start or end parameters 当 start > end 时


(check-false (vector-sorted? < #(1 5 1 0 -1 9 2 4 3))
) ;check-false
(check-true (vector-sorted? < #(1 2 3 4 5))
) ;check-true
(check-false (vector-sorted? < #(1 3 2 4 5))
) ;check-false


;; 带可选参数的测试
(check-true (vector-sorted? < #(5 1 2 3 4) 1)
) ;check-true
(check-false (vector-sorted? < #(5 1 3 2 4) 1)
) ;check-false
(check-true (vector-sorted? < #(5 1 2 3 4) 1 3)
) ;check-true
(check-false (vector-sorted? < #(5 1 3 2 4) 1 4)
) ;check-false


;; 错误处理测试
(check-catch "Invalid start or end parameters"
  (vector-sorted? < #(1 2 3 4 5) 3 2)
) ;check-catch


;; 配合排序函数使用
(check-true (vector-sorted? <
              (vector-sort < #(1 5 1 0 -1 9 2 4 3))
            ) ;vector-sorted?
) ;check-true
(check-true (vector-sorted? <
              (vector-stable-sort <
                #(1 5 1 0 -1 9 2 4 3)
              ) ;vector-stable-sort
            ) ;vector-sorted?
) ;check-true


(check-report)
