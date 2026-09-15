(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator->list
;; 将生成器产出的所有或指定数量的元素收集为列表。
;;
;; 语法
;; ----
;; (generator->list gen)
;; (generator->list gen n)
;;
;; 参数
;; ----
;; gen : procedure
;; 生成器过程。
;;
;; n : exact-nonnegative-integer (可选)
;; 最大收集元素数量。
;;
;; 返回值
;; ----
;; list
;; 收集到的元素列表。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 4 5 6)))
  (check (generator->list g) => '(4 5 6))
)

(let ((g (generator 1 2 3 4 5)))
  (check (generator->list g 3) => '(1 2 3))
)

(let ((g (generator)))
  (check (generator->list g) => '())
)

(check-report)
