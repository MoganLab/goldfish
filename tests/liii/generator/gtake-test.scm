(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gtake
;; 从生成器中获取最多前 k 个元素。支持提供填充值补足不足的元素。
;;
;; 语法
;; ----
;; (gtake gen k)
;; (gtake gen k padding)
;;
;; 参数
;; ----
;; gen : procedure
;; 输入生成器。
;;
;; k : exact-nonnegative-integer
;; 获取的元素个数。
;;
;; padding : any (可选)
;; 若提供 padding，当原始生成器在不足 k 个元素时耗尽，则继续产出 padding 补足到 k 个。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。产出最多 k 个元素后返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gtake (generator 1 2 3) 2)))
  (check (generator->list g) => '(1 2))
)

(let ((g (gtake (make-iota-generator 1024) 3)))
  (check (generator->list g) => '(0 1 2))
)

(let ((g (gtake (generator 1 2) 4 'pad)))
  (check (generator->list g) => '(1 2 pad pad))
)

(let ((g (gtake (generator 1 2) 0)))
  (check-true (eof-object? (g)))
)

(check-report)
