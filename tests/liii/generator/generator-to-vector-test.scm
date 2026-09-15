(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator->vector
;; 将生成器产出的所有或指定数量的元素收集为向量。
;;
;; 语法
;; ----
;; (generator->vector gen)
;; (generator->vector gen n)
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
;; vector
;; 包含生成器产出元素的向量。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 1 2)))
  (check (generator->vector g) => '#(1 2))
)

(let ((g (generator 1 2 3 4 5)))
  (check (generator->vector g 3) => '#(1 2 3))
)

(let ((g (generator)))
  (check (generator->vector g) => '#())
)

(check-report)
