(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator->reverse-list
;; 将生成器产出的所有或指定数量的元素收集为逆序列表。
;;
;; 语法
;; ----
;; (generator->reverse-list gen)
;; (generator->reverse-list gen n)
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
;; 收集到的逆序元素列表。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 1 2 3)))
  (check (generator->reverse-list g) => '(3 2 1))
)

(let ((g (generator 1 2 3 4 5)))
  (check (generator->reverse-list g 3) => '(3 2 1))
)

(let ((g (generator)))
  (check (generator->reverse-list g) => '())
)

(check-report)
