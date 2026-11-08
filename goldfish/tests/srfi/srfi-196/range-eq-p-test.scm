;; range=?
;; 比较多个 range 是否相等。
;;
;; 语法
;; ----
;; (range=? equal r1 r2)
;; (range=? equal r1 r2 ...)
;;
;; 参数
;; ----
;; equal : procedure
;; 比较两个元素是否相等的函数。
;;
;; r1, r2, ... : range
;; 要比较的 range 对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果所有 range 长度相同且对应元素相等，返回 #t，否则返回 #f。
;;
;; 错误处理
;; ----
;; - type-error：当 equal 不是过程，或任一 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r1 (numeric-range 0 5)) (r2 (numeric-range 0 5)) (r3 (numeric-range 1 6)))
  (check-true (range=? = r1 r2))
  (check-false (range=? = r1 r3))
) ;let


(let ((r1 (numeric-range 10 20)) (r2 (numeric-range 10 21)))
  (check-false (range=? = r1 r2))
) ;let

;; 多个 range 的形式也接受校验
(let ((r1 (numeric-range 0 3)) (r2 (numeric-range 0 3)) (r3 (numeric-range 0 3)))
  (check-true (range=? = r1 r2 r3))
  (check-false (range=? = r1 r2 (numeric-range 0 4)))
) ;let

;; equal 不是过程时抛出 type-error
(let ((r1 (numeric-range 0 5)) (r2 (numeric-range 0 5)))
  (check-catch 'type-error (range=? 42 r1 r2))
  (check-catch 'type-error (range=? 'eq r1 r2))
) ;let

;; 任一 r 不是 range 时抛出 type-error
(let ((r (numeric-range 0 5)))
  (check-catch 'type-error (range=? = r 42))
  (check-catch 'type-error (range=? = "r" r))
  (check-catch 'type-error (range=? = r 42 42))
) ;let


(check-report)
