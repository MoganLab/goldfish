(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

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
;; 示例
;; ----
;; (range-fold + 0 (numeric-range 1 6)) => 15
;; (range-fold * 1 (numeric-range 1 6)) => 120
;;
;; 错误处理
;; ----
;; 无

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

(check-report)
