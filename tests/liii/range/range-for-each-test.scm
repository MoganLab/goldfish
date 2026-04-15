(import (liii check) (liii range))


(check-set-mode! 'report-failed)


;; range-for-each
;; 对 range 的每个元素执行副作用操作。
;;
;; 语法
;; ----
;; (range-for-each proc r)
;;
;; 参数
;; ----
;; proc : procedure
;; 副作用函数，接受一个元素。
;;
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; undefined
;; 无（未定义）。
;;
;; 示例
;; ----
;; (range-for-each (lambda (x) (display x)) (numeric-range 0 5))
;;
;; 错误处理
;; ----
;; 无


(let ((r (numeric-range 0 5)) (result '()))
  (range-for-each (lambda (x)
                    (set! result (cons x result))
                  ) ;lambda
    r
  ) ;range-for-each
  (check result => '(4 3 2 1 0))
) ;let


(let ((r (numeric-range 0 0)) (result '()))
  (range-for-each (lambda (x)
                    (set! result (cons x result))
                  ) ;lambda
    r
  ) ;range-for-each
  (check result => '())
) ;let


(let ((r (numeric-range 1 4)) (sum 0))
  (range-for-each (lambda (x) (set! sum (+ sum x)))
    r
  ) ;range-for-each
  (check sum => 6)
) ;let


(check-report)
