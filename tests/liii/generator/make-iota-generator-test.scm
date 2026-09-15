(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; make-iota-generator
;; 创建一个产出算术级数（等差数列）的有限生成器。
;;
;; 语法
;; ----
;; (make-iota-generator count)
;; (make-iota-generator count start)
;; (make-iota-generator count start step)
;;
;; 参数
;; ----
;; count : integer
;; 产出元素的个数。
;;
;; start : number (可选)
;; 起始数值，默认为 0。
;;
;; step : number (可选)
;; 步长值，默认为 1。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。产出 count 个元素后返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (make-iota-generator 3 10 2)))
  (check (g) => 10)
  (check (g) => 12)
  (check (g) => 14)
  (check-true (eof-object? (g)))
)

(let ((g (make-iota-generator 3)))
  (check (g) => 0)
  (check (g) => 1)
  (check (g) => 2)
  (check-true (eof-object? (g)))
)

(let ((g (make-iota-generator 2 5)))
  (check (g) => 5)
  (check (g) => 6)
  (check-true (eof-object? (g)))
)

(check-report)
