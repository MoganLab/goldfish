(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; vector-accumulator!
;; 创建原地填充指定向量的累加器。
;;
;; 语法
;; ----
;; (vector-accumulator! vec at)
;;
;; 参数
;; ----
;; vec : vector
;; 目标填充向量。
;;
;; at : exact-nonnegative-integer
;; 写入 vec 的起始索引。
;;
;; 返回值
;; ----
;; procedure
;; 一个单参累加器过程。传入普通元素时依次在 vec 的相应索引位置赋值；传入 eof-object 时返回该向量 vec。
;;
;; 说明
;; ----
;; 若需收集元素并直接返回新分配的向量，参见：
;;   gf doc "vector-accumulator"
;;
;; 错误处理
;; ----
;; 若写入元素数量超出向量剩余容量，可能引发越界错误。

(let* ((v #(0 1 0 0 0))
       (a (vector-accumulator! v 0)))
  (a 2)
  (check v => '#(2 1 0 0 0))
  (a 2)
  (a 2)
  (check v => '#(2 2 2 0 0))
  (a 2)
  (check (a (eof-object)) => '#(2 2 2 2 0))
)

(let* ((v #(0 1 0 0 0))
       (a (vector-accumulator! v 1)))
  (a 2)
  (check v => '#(0 2 0 0 0))
  (a 2)
  (a 2)
  (check (a (eof-object)) => '#(0 2 2 2 0))
)

(check-report)
