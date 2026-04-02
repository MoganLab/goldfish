(import (liii check)) ;import

(check-set-mode! 'report-failed)

;; check:proc
;; 底层断言过程，可传入自定义比较函数。
;;
;; 语法
;; ----
;; (check:proc expression thunk expected-result [equal])
;;
;; 参数
;; ----
;; expression : any
;; 用于报告的原始表达式。
;; thunk : procedure?
;; 无参过程，用于产生实际结果。
;; expected-result : any
;; 期望结果。
;; equal : procedure?
;; 可选比较函数，接受 `(actual expected)` 两个参数。

(check:proc '(list 1 2 3)
            (lambda () (list 1 2 3))
            '(1 2 3))

(check:proc '(+ 0.1 0.2)
            (lambda () (+ 0.1 0.2))
            0.3
            (lambda (actual expected)
              (<= (abs (- actual expected)) 1e-12))
) ;check:proc

(check-report)
