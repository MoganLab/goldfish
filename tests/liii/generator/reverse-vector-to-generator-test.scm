(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; reverse-vector->generator
;; 将向量逆序转换为生成器，从后往前产出向量中的元素。
;;
;; 语法
;; ----
;; (reverse-vector->generator vec)
;; (reverse-vector->generator vec start)
;; (reverse-vector->generator vec start end)
;;
;; 参数
;; ----
;; vec : vector
;; 待逆序转换的向量。
;;
;; start : exact-nonnegative-integer (可选)
;; 起始索引（包含），默认为 0。
;;
;; end : exact-nonnegative-integer (可选)
;; 结束索引（不包含），默认为 (vector-length vec)。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。从 end - 1 逆序递减至 start 依次产出元素，耗尽时返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (reverse-vector->generator '#(1 2))))
  (check (g) => 2)
  (check (g) => 1)
  (check-true (eof-object? (g)))
)

(let ((g (reverse-vector->generator '#(10 20 30 40) 1 3)))
  (check (g) => 30)
  (check (g) => 20)
  (check-true (eof-object? (g)))
)

(let ((g (reverse-vector->generator '#(10 20 30) 1)))
  (check (g) => 30)
  (check (g) => 20)
  (check-true (eof-object? (g)))
)

(check-report)
