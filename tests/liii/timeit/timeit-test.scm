(import (liii check)
        (liii timeit)
        (liii time)
) ;import

(check-set-mode! 'report-failed)

;; timeit
;; 测量代码执行时间。
;;
;; 语法
;; ----
;; (timeit stmt setup number)
;;
;; 参数
;; ----
;; stmt : (lambda () any)
;;   要测量执行时间的语句，接受一个无参数的 lambda 表达式。
;; setup : (lambda () any)
;;   初始化代码，接受一个无参数的 lambda 表达式，只执行一次。
;; number : integer?
;;   stmt 执行的次数，必须是正整数。
;;
;; 返回值
;; ----
;; number?
;;   返回执行 stmt number 次的总时间，以秒为单位的浮点数。
;;
;; 描述
;; ----
;; 1. 首先执行一次 setup 函数进行初始化
;; 2. 然后执行 stmt 函数 number 次
;; 3. 返回总执行时间（秒）
;; 4. 主要用于性能测试和代码优化
;; 5. 时间测量精度取决于系统实现

;; Test basic timeit functionality
(let ((result (timeit (lambda () (+ 1 2))
                      (lambda () #t)
                      1000)))
  (check (number? result) => #t)
  (check (>= result 0) => #t)
) ;let

;; Test timeit with setup function
(let ((counter 0))
  (timeit (lambda () (set! counter (+ counter 1)))
          (lambda () (set! counter 0))
          100
  ) ;timeit
  (check (= counter 100) => #t)
) ;let

;; Test timeit with different number of iterations
(let ((result1 (timeit (lambda () (* 2 3))
                       (lambda () #t)
                       100))
      (result2 (timeit (lambda () (* 2 3))
                       (lambda () #t)
                       1000))
      ) ;result2
  (check (number? result1) => #t)
  (check (number? result2) => #t)
  (check (>= result2 result1) => #t)
) ;let

;; Test timeit with empty setup
(let ((result (timeit (lambda () (display ""))
                      (lambda () #t)
                      10)))
  (check (number? result) => #t)
  (check (>= result 0) => #t)
) ;let

;; Test timeit with complex setup
(let ((lst '()))
  (timeit (lambda () (set! lst (cons 'x lst)))
          (lambda () (set! lst (make-list 100 'a)))
          50
  ) ;timeit
  (check (= (length lst) 150) => #t)
) ;let

;; Test error handling - invalid number parameter
(check-catch 'type-error (timeit (lambda () #t)
                                 (lambda () #t)
                                 'invalid)
) ;check-catch

;; Test error handling - invalid stmt parameter
(check-catch 'type-error (timeit 'not-a-lambda
                                 (lambda () #t)
                                 100)
) ;check-catch

;; Test error handling - invalid setup parameter
(check-catch 'type-error (timeit (lambda () #t)
                                 'not-a-lambda
                                 100)
) ;check-catch

;; Test timeit with sleep to verify timing accuracy
(let ((result (timeit (lambda () (sleep 0.1))
                      (lambda () #t)
                      1)))
  (check (number? result) => #t)
  (check (>= result 0.09) => #t)
) ;let

;; Test timeit with multiple sleep iterations
(let ((result (timeit (lambda () (sleep 0.01))
                      (lambda () #t)
                      5)))
  (check (number? result) => #t)
  (check (>= result 0.04) => #t)
) ;let

;; Test timeit with very short sleep
(let ((result (timeit (lambda () (sleep 0.001))
                      (lambda () #t)
                      10)))
  (check (number? result) => #t)
  (check (>= result 0.005) => #t)
) ;let

(check-report)
