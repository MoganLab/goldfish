(import (liii check) (liii par) (liii go) (liii list) (liii time) (scheme time))

(check-set-mode! 'report-failed)

;; vector-par-for-each
;;
;; 语法
;; ----
;; (vector-par-for-each f vec)
;;
;; 参数
;; ----
;; f : procedure
;; 并发执行的过程，接受向量的一个元素作为参数。
;; f 会在后台 worker 线程的独立环境中运行，其参数经序列化传输。
;;
;; vec : vector
;; 待遍历的向量，元素须为可序列化的数据。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 基于 (liii go) 线程池分批并发执行，各元素的执行顺序不确定。
;; 2. 向量分批分块（Chunking）派发至各 worker，开销为 O(P) 而非 O(N)。
;; 3. 主线程会等待所有任务执行完毕后才返回。
;; 4. 若 vec 为空向量，直接返回 unspecified，不启动后台任务。
;; 5. 若后台 worker 发生未捕获异常，会在主线程重新抛出该异常。

;; 1. 空向量测试
(check (vector-par-for-each (lambda (x) x) #()) => (if #f #f))

;; 2. 使用命名过程并发执行

(define ch1 (make-chan 10))

(define (test-square x)
  (chan-send! ch1 (* x x))
) ;define

(vector-par-for-each test-square #(1 2 3 4 5))

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

(vector-par-for-each (lambda (x) (chan-send! ch2 (+ x 100))) #(10 20 30))

(define results2 (list (chan-recv! ch2) (chan-recv! ch2) (chan-recv! ch2)))

(check (sort! results2 <) => '(110 120 130))

;; 4. 闭包自由变量捕获测试

(define ch3 (make-chan 5))

(let ((base 50))
  (vector-par-for-each (lambda (x) (chan-send! ch3 (+ x base))) #(1 2 3))
) ;let

(define results3 (list (chan-recv! ch3) (chan-recv! ch3) (chan-recv! ch3)))

(check (sort! results3 <) => '(51 52 53))

;; 5. 参数类型校验（type-error）
(check-catch 'type-error (vector-par-for-each 123 #(1 2 3)))
(check-catch 'type-error (vector-par-for-each "not-a-proc" #(1 2 3)))
(check-catch 'type-error (vector-par-for-each (lambda (x) x) 123))
(check-catch 'type-error (vector-par-for-each (lambda (x) x) "not-a-vector"))
(check-catch 'type-error (vector-par-for-each (lambda (x) x) '(1 2 3)))
(check-catch 'type-error (vector-par-for-each display #(1 2 3)))

;; 6. 异常传播测试
(check-catch 'value-error
  (vector-par-for-each (lambda (x) (if (= x 2) (error 'value-error "error in worker") x))
    #(1 2 3)
  ) ;vector-par-for-each
) ;check-catch

;; 7. 并发执行加速验证
(when (>= (go-worker-count) 4)
  ;; 先热身，触发线程池与 worker 初始化，避免一次性启动开销计入计时窗口
  (vector-par-for-each (lambda (x) x) #(1))
  (let ((t0 (current-jiffy)))
    (vector-par-for-each (lambda (x) (sleep 0.1)) #(1 2 3 4))
    (let* ((dt (/ (- (current-jiffy) t0) (jiffies-per-second))) (ms (* dt 1000.0)))
      (check (< ms 350) => #t)
    ) ;let*
  ) ;let
) ;when

(check-report)
