(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; do
;; 迭代循环构造。
;;
;; 语法
;; ----
;; (do ((var init step) ...) (test-expr result) body ...)
;;
;; 参数
;; ----
;; var : symbol
;; 循环变量名。
;;
;; init : any
;; 循环变量初始值。
;;
;; step : any
;; 每次迭代后更新循环变量的表达式。
;;
;; test-expr : boolean expression
;; 测试表达式，为真时退出循环。
;;
;; result : any
;; 循环结束时返回的值。
;;
;; body ... : any
;; 循环体，每次迭代执行（除非 test-expr 为真）。
;;
;; 返回值
;; -----
;; any
;; 返回 result 表达式的结果。

;; 基础测试 - 简单计数
(check
  (do ((i 0 (+ i 1)))
      ((= i 5) i))
  => 5)

;; 累加计算
(check
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((= i 5) sum))
  => 10)

;; 手动更新循环变量
(check
  (do ((i 0))
      ((= i 5) i)
    (set! i (+ i 1)))
  => 5)

;; 使用循环体 - 填充向量
(check
  (let ((vec (make-vector 5)))
    (do ((i 0 (+ i 1)))
        ((= i 5) vec)
      (vector-set! vec i i)))
  => #(0 1 2 3 4))

;; 多变量迭代
(check
  (do ((i 0 (+ i 1))
       (j 10 (- j 1)))
      ((= i 5) (list i j)))
  => '(5 5))

;; 嵌套 do
(check
  (do ((i 0 (+ i 1)))
      ((= i 3) i)
    (do ((j 0 (+ j 1)))
        ((= j 2))))
  => 3)

;; 空循环体
(check
  (do ((i 0 (+ i 1)))
      ((= i 0) 'done))
  => 'done)

(check-report)
