;; range-fold-right
;; 对 range 进行右折叠。
;;
;; 语法
;; ----
;; (range-fold-right proc nil r)
;;
;; 参数
;; ----
;; proc : procedure
;; 折叠函数，接受当前元素和累加值。
;;
;; nil : any
;; 初始累加值。
;;
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; any
;; 最终的累加值。
;;
;; 错误处理
;; ----
;; - type-error：当 proc 不是过程，或任一 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 1 6)))
  (check (range-fold-right cons '() r) => '(1 2 3 4 5))
) ;let


(let ((r (numeric-range 0 0)))
  (check (range-fold-right cons '() r) => '())
) ;let


(let ((r (numeric-range 1 4)))
  (check (range-fold-right (lambda (x acc) (cons x acc)) '() r) => '(1 2 3))
) ;let


(let ((r (numeric-range 1 6)))
  (check (range-fold-right + 0 r) => 15)
) ;let



;; proc 不是过程时抛出 type-error
(check-catch 'type-error (range-fold-right 42 0 (numeric-range 0 5)))
(check-catch 'type-error (range-fold-right "not-a-proc" 0 (numeric-range 0 5)))
(check-catch 'type-error (range-fold-right 'proc 0 (numeric-range 0 5)))

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-fold-right + 0 42))
(check-catch 'type-error (range-fold-right + 0 "not-a-range"))

;; 多 range 形式同样校验
(check-catch 'type-error
  (range-fold-right + 0 (numeric-range 0 3) "not-a-range")
) ;check-catch

(check-report)
