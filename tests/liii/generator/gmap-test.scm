(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gmap
;; 对一个或多个生成器产出的元素应用转换过程，产生映射后的新生成器。
;;
;; 语法
;; ----
;; (gmap proc gen ...)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射转换过程，参数个数与输入生成器的数量相同。
;;
;; gen ... : procedure
;; 一个或多个输入生成器。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。每次调用将 proc 作用于各生成器产出的当前元素并返回其结果；任一生成器耗尽时返回 eof-object。
;;
;; 错误处理
;; ----
;; 若未提供生成器参数，会抛出错误。

(let ((g (gmap (lambda (x) (* x x)) (generator 1 2 3))))
  (check (g) => 1)
  (check (g) => 4)
  (check (g) => 9)
  (check-true (eof-object? (g)))
)

(let ((g (gmap + (generator 1 2 3) (generator 10 20))))
  (check (generator->list g) => '(11 22))
)

(check-report)
