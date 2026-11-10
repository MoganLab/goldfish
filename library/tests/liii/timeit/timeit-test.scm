;; timeit
;; 循环执行给定语句，测量其耗时（秒）。
;;
;; 语法
;; ----
;; (timeit stmt [setup [number]])
;;
;; 参数
;; ----
;; stmt : procedure?
;; 被测语句，无参过程，将被循环执行 number 次。
;; setup : procedure? 或 '()
;; 可选。计时开始前执行一次的准备过程，'() 表示不做准备，默认 '()。
;; number : 正整数
;; 可选。stmt 的执行次数，必须为正整数，默认 1000000。
;;
;; 返回值
;; ----
;; real?
;; 执行 number 次 stmt 所经过的墙钟时间（秒），非负实数。
;;
;; 说明
;; ----
;; timeit 先在 setup 非 '() 时调用 (setup)，随后记录起始时刻，
;; 循环执行 (stmt) 共 number 次，最后返回结束与起始时刻之差。
;;
;; 错误处理
;; ----
;; 以下情形抛出 type-error：
;; stmt 不是过程；setup 既不是过程也不是 '()；number 不是正整数。

(import (liii check) (liii timeit) (liii time))

(check-set-mode! 'report-failed)

;; 基本用法：返回非负实数
(let ((result (timeit (lambda () (+ 1 2)) (lambda () #t) 1000)))
  (check (number? result) => #t)
  (check (>= result 0) => #t)
) ;let

;; setup 先执行一次，stmt 执行 number 次
(let ((counter 0))
  (timeit (lambda () (set! counter (+ counter 1)))
    (lambda () (set! counter 0))
    100
  ) ;timeit
  (check (= counter 100) => #t)
) ;let

;; 不同次数均返回合法结果
(let ((result1 (timeit (lambda () (* 2 3)) (lambda () #t) 100))
      (result2 (timeit (lambda () (* 2 3)) (lambda () #t) 1000))
     ) ;
  (check (number? result1) => #t)
  (check (number? result2) => #t)
  (check (>= result1 0) => #t)
  (check (>= result2 0) => #t)
) ;let

;; 被测语句可为任意过程
(let ((result (timeit (lambda () (display "")) (lambda () #t) 10)))
  (check (number? result) => #t)
  (check (>= result 0) => #t)
) ;let

;; setup 只在计时前执行一次，不计入循环
(let ((lst '()))
  (timeit (lambda () (set! lst (cons 'x lst)))
    (lambda () (set! lst (make-list 100 'a)))
    50
  ) ;timeit
  (check (= (length lst) 150) => #t)
) ;let

;; 错误处理：number 非法
(check-catch 'type-error (timeit (lambda () #t) (lambda () #t) 'invalid))

;; 错误处理：stmt 不是过程
(check-catch 'type-error (timeit 'not-a-lambda (lambda () #t) 100))

;; 错误处理：setup 既非过程也非 '()
(check-catch 'type-error (timeit (lambda () #t) 'not-a-lambda 100))

;; 耗时下限：sleep 0.1 秒至少累计 0.09 秒
(let ((result (timeit (lambda () (sleep 0.1)) (lambda () #t) 1)))
  (check (number? result) => #t)
  (check (>= result 0.09) => #t)
) ;let

(let ((result (timeit (lambda () (sleep 0.01)) (lambda () #t) 5)))
  (check (number? result) => #t)
  (check (>= result 0.04) => #t)
) ;let

(let ((result (timeit (lambda () (sleep 0.001)) (lambda () #t) 10)))
  (check (number? result) => #t)
  (check (>= result 0.005) => #t)
) ;let

(check-report)
