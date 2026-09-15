(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gdrop
;; 跳过生成器中的前 k 个元素，随后产出后续元素。
;;
;; 语法
;; ----
;; (gdrop gen k)
;;
;; 参数
;; ----
;; gen : procedure
;; 输入生成器。
;;
;; k : exact-nonnegative-integer
;; 需跳过的元素个数。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。跳过前 k 个元素后，依次产出剩余元素。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gdrop (generator 1 2 3) 2)))
  (check (generator->list g) => '(3))
)

(let ((g (gdrop (generator 1 2 3) 0)))
  (check (generator->list g) => '(1 2 3))
)

(let ((g (gdrop (generator 1 2 3) 5)))
  (check (generator->list g) => '())
)

(check-report)
