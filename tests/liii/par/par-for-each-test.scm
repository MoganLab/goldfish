(import (liii check) (liii par) (liii go) (liii list) (scheme time))

(check-set-mode! 'report-failed)

;; par-for-each
;;
;; 语法
;; ----
;; (par-for-each f l)
;;
;; 参数
;; ----
;; f : procedure
;; 并发执行的过程，接受列表的一个元素作为参数。
;; f 会在后台 worker 线程的独立环境中运行，其参数经序列化传输。
;;
;; l : list
;; 待遍历的列表，列表元素须为可序列化的数据。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 基于 (liii go) 线程池并发执行，各元素的执行顺序不确定。
;; 2. 主线程会等待所有任务执行完毕后才返回。
;; 3. 若 l 为空列表，直接返回 unspecified，不启动后台任务。
;; 4. 若后台 worker 发生未捕获异常，会在主线程重新抛出该异常。

;; 1. 空列表测试
(check (par-for-each (lambda (x) x) '()) => (if #f #f))

;; 2. 使用命名过程并发执行

(define ch1 (make-chan 10))

(define (test-square x)
  (chan-send! ch1 (* x x))
) ;define

(par-for-each test-square '(1 2 3 4 5))

(define results1
  (list (chan-recv! ch1)
    (chan-recv! ch1)
    (chan-recv! ch1)
    (chan-recv! ch1)
    (chan-recv! ch1)
  ) ;list
) ;define

(check (sort! results1 <) => '(1 4 9 16 25))

;; 3. 使用匿名 lambda 并发执行

(define ch2 (make-chan 5))

(par-for-each (lambda (x) (chan-send! ch2 (+ x 100))) '(10 20 30))

(define results2 (list (chan-recv! ch2) (chan-recv! ch2) (chan-recv! ch2)))

(check (sort! results2 <) => '(110 120 130))

;; 4. 参数类型校验（type-error）
(check-catch 'type-error (par-for-each 123 '(1 2 3)))
(check-catch 'type-error (par-for-each "not-a-proc" '(1 2 3)))
(check-catch 'type-error (par-for-each (lambda (x) x) 123))
(check-catch 'type-error (par-for-each (lambda (x) x) "not-a-list"))
(check-catch 'type-error (par-for-each (lambda (x) x) #(1 2 3)))
(check-catch 'type-error (par-for-each display '(1 2 3)))

;; 5. 异常传播测试
(check-catch 'value-error
  (par-for-each (lambda (x) (if (= x 2) (error 'value-error "error in worker") x))
    '(1 2 3)
  ) ;par-for-each
) ;check-catch

;; 6. 并发执行加速验证
(when (>= (go-worker-count) 4)
  (let ((t0 (current-jiffy)))
    (par-for-each (lambda (x) (g_msleep 100)) '(1 2 3 4))
    (let* ((dt (/ (- (current-jiffy) t0) (jiffies-per-second))) (ms (* dt 1000.0)))
      (check (< ms 350) => #t)
    ) ;let*
  ) ;let
) ;when

(check-report)
