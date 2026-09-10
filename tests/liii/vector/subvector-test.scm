(import (liii check) (liii vector))

(check-set-mode! 'report-failed)

;; subvector
;; 创建与原向量共享数据存储的子向量（切片）。
;;
;; 语法
;; ----
;; (subvector orig)
;; (subvector orig start)
;; (subvector orig start end)
;; (subvector orig start end dimensions)
;;
;; 参数
;; ----
;; orig       : vector?
;;              要截取子向量的目标向量（支持通用 vector 及 int-vector、float-vector 等特化向量）。子向量将与该目标向量共享底层内存。
;; start      : exact-nonnegative-integer?，可选，默认值为 0
;;              切片在原向量中的起始索引（从 0 开始计数，包含该位置）。
;; end        : exact-nonnegative-integer?，可选，默认值为原向量的元素总数
;;              切片在原向量中的结束索引（不包含该位置，与 start 构成半开区间 [start, end)）。切片包含的元素总数为 end - start。
;; dimensions : (list-of exact-positive-integer?)，可选
;;              指定新子向量的形状与维度大小（例如 '(3 2) 表示 3 行 2 列）。若省略则返回长度为 end - start 的一维向量；若指定，列表中所有维度的乘积必须严格等于切片元素总数 end - start。
;;
;; 返回值
;; ----
;; vector
;; 与原向量在指定区间共享底层内存的子向量（满足 subvector? 与 vector?）。
;;
;; 注意
;; ----
;; 1. 共享内存特性：修改子向量的元素会同步影响原向量，反之亦然。
;; 2. 配套操作：可用 subvector? 判断是否为子向量，用 subvector-vector 获取原向量，用 subvector-position 获取在原向量中的偏移量。
;;
;; 错误处理
;; ----
;; type-error: orig 不是向量、start/end 不是整数或 dimensions 不是整数列表时抛出
;; out-of-range: start/end 为负数、超出原向量范围或 start > end 时抛出
;; value-error: dimensions 各维度乘积与切片区间长度不一致时抛出

;; 基本功能与默认参数
(let ((v #(1 2 3 4 5 6)))
  (check (subvector? (subvector v 0 3)) => #t)
  (check (subvector-position (subvector v 2 4)) => 2)
  (check (subvector-vector (subvector v 2 4)) => v)
  (check (vector->list (subvector v)) => '(1 2 3 4 5 6))
  (check (vector->list (subvector v 2)) => '(3 4 5 6))
  (check (vector->list (subvector v 1 4)) => '(2 3 4))
  (check (vector-dimensions (subvector v 0 6 '(3 2))) => '(3 2))
  (check (vector-ref (subvector v 0 6 '(3 2)) 1 1) => 4)
) ;let

;; 修改 subvector 元素会反映到原 vector（共享存储）
(let ((v (vector 1 2 3 4)))
  (let ((sub (subvector v 1 3)))
    (vector-set! sub 0 99)
    (check (vector-ref v 1) => 99)
  ) ;let
) ;let

;; 边界错误
(let ((v #(1 2 3)))
  (check-catch 'type-error (subvector 'not-a-vector))
  (check-catch 'out-of-range (subvector v -1 2))
  (check-catch 'out-of-range (subvector v 0 4))
  (check-catch 'out-of-range (subvector v 2 1))
) ;let

(let ((v #(1 2 3 4 5 6)))
  (check-catch 'value-error (subvector v 0 6 '(2 2)))
  (check-catch 'value-error (subvector v 0 6 '(3 3)))
  (check-catch 'value-error (subvector v 1 5 '(3 2)))
  (check-catch 'type-error (subvector v 0 6 '((1 2) (3 4))))
  (check-catch 'type-error (subvector v 0 6 '(7 1)))
  (check-catch 'type-error (subvector v 0 6 '(-1 1)))
) ;let

(check-report)
