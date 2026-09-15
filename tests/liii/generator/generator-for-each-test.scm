(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator-for-each
;; 对一个或多个生成器产出的每一组元素依次执行过程（执行副作用）。
;;
;; 语法
;; ----
;; (generator-for-each proc gen ...)
;;
;; 参数
;; ----
;; proc : procedure
;; 副作用过程，参数个数与输入生成器数量相等。
;;
;; gen ... : procedure
;; 一个或多个输入生成器。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定的值。当任意生成器耗尽时停止。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 1 2 3))
      (sum 0))
  (generator-for-each (lambda (x) (set! sum (+ sum x))) g)
  (check sum => 6)
)

(let ((g1 (generator 1 2 3))
      (g2 (generator 10 20 30))
      (res '()))
  (generator-for-each (lambda (x y) (set! res (cons (+ x y) res))) g1 g2)
  (check res => '(33 22 11))
)

(check-report)
