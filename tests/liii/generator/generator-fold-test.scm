(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator-fold
;; 对一个或多个生成器执行左折叠操作。
;;
;; 语法
;; ----
;; (generator-fold proc seed gen ...)
;;
;; 参数
;; ----
;; proc : procedure
;; 接收各生成器当前值以及当前累计 seed 的过程：(proc v ... seed)。
;;
;; seed : any
;; 初始累计值。
;;
;; gen ... : procedure
;; 一个或多个输入生成器。
;;
;; 返回值
;; ----
;; any
;; 最终折叠结果。当任意生成器耗尽时停止。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 1 2 3)))
  (check (generator-fold + 0 g) => 6)
)

(let ((g (generator 1 2 3)))
  (check (generator-fold * 1 g) => 6)
)

(let ((g1 (generator 1 2 3))
      (g2 (generator 10 20 30)))
  (check (generator-fold (lambda (x y acc) (+ acc (* x y))) 0 g1 g2) => 140)
)

(check-report)
