(import (liii check) (liii par) (liii go) (liii time) (scheme time))

(check-set-mode! 'report-failed)

;; vector-par-map
;;
;; 语法
;; ----
;; (vector-par-map f vec)
;;
;; 参数
;; ----
;; f : procedure
;; 并发映射的过程，接受向量的一个元素作为参数，返回映射后的结果。
;; f 会在后台 worker 线程的独立环境中运行，其参数与返回值经序列化传输。
;;
;; vec : vector
;; 待映射的向量，元素须为可序列化的数据。
;;
;; 返回值
;; ----
;; vector
;; 与输入向量等长且严格保序的映射结果向量。
;;
;; 说明
;; ----
;; 1. 基于 (liii go) 线程池分批并发执行，严格保证返回结果向量的元素顺序与原向量一致。
;; 2. 向量分批分块（Chunking）切块，内部本地映射并批量装配，通道开销仅为 O(P)。
;; 3. 若 vec 为空向量，直接返回 #()，不启动后台任务。
;; 4. 主线程会等待所有 worker 执行完毕后才返回。
;; 5. 若后台 worker 发生未捕获异常，主线程在收敛全部任务后重新抛出最早捕获的异常。
;; 6. 若参数类型不匹配，抛出 type-error。

;; 1. 空向量测试
(check (vector-par-map (lambda (x) (* x x)) #()) => #())

;; 2. 使用命名过程并发保序映射

(define (test-square x)
  (* x x)
) ;define

(check (vector-par-map test-square #(1 2 3 4 5)) => #(1 4 9 16 25))

;; 3. 使用匿名 lambda 并发保序映射
(check (vector-par-map (lambda (x) (+ x 100)) #(10 20 30)) => #(110 120 130))

;; 4. 逆序耗时严格保序测试
;; 各元素模拟不同耗时（首元素耗时最长，尾元素耗时最短），验证即使完成乱序，结果仍然严格保序

(define order-res
  (vector-par-map
    (lambda (x) (sleep (* (- 5 x) 0.02)) (* x x))
    #(1 2 3 4)
  ) ;vector-par-map
) ;define
(check order-res => #(1 4 9 16))

;; 5. 闭包自由变量捕获测试

(define factor-res
  (let ((factor 10))
    (vector-par-map (lambda (x) (* x factor)) #(1 2 3))
  ) ;let
) ;define
(check factor-res => #(10 20 30))

;; 6. 参数类型校验（type-error）
(check-catch 'type-error (vector-par-map 123 #(1 2 3)))
(check-catch 'type-error (vector-par-map "not-a-proc" #(1 2 3)))
(check-catch 'type-error (vector-par-map (lambda (x) x) 123))
(check-catch 'type-error (vector-par-map (lambda (x) x) "not-a-vector"))
(check-catch 'type-error (vector-par-map (lambda (x) x) '(1 2 3)))
(check-catch 'type-error (vector-par-map display #(1 2 3)))

;; 7. 异常传播与孤儿任务防护测试
(check-catch 'worker-err
  (vector-par-map (lambda (x) (if (= x 2) (error 'worker-err "worker-failed") (* x 10)))
    #(1 2 3)
  ) ;vector-par-map
) ;check-catch

;; 8. 大向量分批切块映射正确性

(define big-n 100)

(define big-vec (make-vector big-n))

(define expected-big (make-vector big-n))

(let loop
  ((i 0))
  (when (< i big-n)
    (vector-set! big-vec i i)
    (vector-set! expected-big i (* i 2))
    (loop (+ i 1))
  ) ;when
) ;let

(check (vector-par-map (lambda (x) (* x 2)) big-vec) => expected-big)

;; 9. 并发加速效果验证
(when (>= (go-worker-count) 4)
  ;; 先热身，触发线程池与 worker 初始化，避免一次性启动开销计入计时窗口
  (vector-par-map (lambda (x) x) #(1))
  (let ((t0 (current-jiffy)))
    (define res (vector-par-map (lambda (x) (sleep 0.1) (* x 2)) #(1 2 3 4)))
    (let* ((dt (/ (- (current-jiffy) t0) (jiffies-per-second))) (ms (* dt 1000.0)))
      (check res => #(2 4 6 8))
      (check (< ms 350) => #t)
    ) ;let*
  ) ;let
) ;when

(check-report)
