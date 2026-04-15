(import (liii check) (liii range))


(check-set-mode! 'report-failed)


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
;; 示例
;; ----
;; (range-fold-right cons '() (numeric-range 1 6)) => '(1 2 3 4 5)
;;
;; 错误处理
;; ----
;; 无


(let ((r (numeric-range 1 6)))
  (check (range-fold-right cons '() r)
    =>
    '(1 2 3 4 5)
  ) ;check
) ;let


(let ((r (numeric-range 0 0)))
  (check (range-fold-right cons '() r)
    =>
    '()
  ) ;check
) ;let


(let ((r (numeric-range 1 4)))
  (check (range-fold-right (lambda (x acc) (cons x acc))
           '()
           r
         ) ;range-fold-right
    =>
    '(1 2 3)
  ) ;check
) ;let


(let ((r (numeric-range 1 6)))
  (check (range-fold-right + 0 r) => 15)
) ;let


(check-report)
