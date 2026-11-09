;; range-fold
;; 对 range 进行左折叠。
;;
;; 语法
;; ----
;; (range-fold proc nil r)
;;
;; 参数
;; ----
;; proc : procedure
;; 折叠函数，接受累加值和当前元素。
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
  (check (range-fold + 0 r) => 15)
  (check (range-fold * 1 r) => 120)
) ;let


(let ((r (numeric-range 0 5)))
  (check (range-fold + 10 r) => 20)
) ;let


(let ((r (numeric-range 0 0)))
  (check (range-fold + 0 r) => 0)
  (check (range-fold * 1 r) => 1)
) ;let


(let ((r (numeric-range 1 4)))
  (check (range-fold (lambda (acc x) (cons x acc)) '() r) => '(3 2 1))
) ;let



;; proc 不是过程时抛出 type-error
(check-catch 'type-error (range-fold 42 0 (numeric-range 0 5)))
(check-catch 'type-error (range-fold "not-a-proc" 0 (numeric-range 0 5)))
(check-catch 'type-error (range-fold 'proc 0 (numeric-range 0 5)))

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-fold + 0 42))
(check-catch 'type-error (range-fold + 0 "not-a-range"))

;; 多 range 形式同样校验
(check-catch 'type-error (range-fold + 0 (numeric-range 0 3) "not-a-range"))

(check-report)
