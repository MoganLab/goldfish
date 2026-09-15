(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator->vector!
;; 从指定索引位置开始，将生成器产出的元素按顺序填充到给定的可变向量中。
;;
;; 语法
;; ----
;; (generator->vector! vector at gen)
;;
;; 参数
;; ----
;; vector : vector
;; 目标填充向量。
;;
;; at : exact-nonnegative-integer
;; 写入 vector 的起始索引。
;;
;; gen : procedure
;; 元素生成器。
;;
;; 返回值
;; ----
;; exact-nonnegative-integer
;; 实际写入向量中的元素数量。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 1 2 3)) (v (make-vector 3)))
  (let ((count (generator->vector! v 0 g)))
    (check count => 3)
    (check v => '#(1 2 3))
  )
)

(let ((g (generator 1 2 3)) (v (make-vector 3 #f)))
  (let ((count (generator->vector! v 1 g)))
    (check count => 2)
    (check v => '#(#f 1 2))
  )
)

(check-report)
