(import (liii check) (liii par) (liii go) (liii time) (scheme time))

(check-set-mode! 'report-failed)

;; vector-par-filter
;;
;; 语法
;; ----
;; (vector-par-filter pred vec)
;;
;; 参数
;; ----
;; pred : procedure
;; 用于判断向量元素的单参谓词过程。
;; pred 会在后台 worker 线程的独立环境中运行，其参数经序列化传输。
;; 返回非 #f 的值均视为真值并保留对应元素。
;;
;; vec : vector
;; 待过滤的向量，元素须为可序列化的数据。
;;
;; 返回值
;; ----
;; vector
;; 仅保留满足谓词条件（即 (pred elem) 返回非 #f）的元素向量，严格保持在原向量中的相对次序。
;;
;; 说明
;; ----
;; 1. 基于 (liii go) 线程池分批并发执行，严格保证筛选结果元素的相对顺序与原向量一致。
;; 2. 向量分批分块（Chunking）切块，内部本地过滤并保序拼接，通道开销仅为 O(P)。
;; 3. 若 vec 为空向量，直接返回 #()，不启动后台任务。
;; 4. 谓词判定支持 Scheme 广义真值：除 #f 外，所有返回值（如数字、符号、字符串、空列表等）均被视为真值。
;; 5. 主线程会等待所有 worker 执行完毕后才返回。
;; 6. 若后台 worker 发生未捕获异常，主线程在收敛全部任务后重新抛出最早捕获的异常。
;; 7. 若参数类型不匹配，抛出 type-error。

;; 1. 空向量测试
(check (vector-par-filter (lambda (x) (even? x)) #()) => #())

;; 2. 基础过滤与命名过程/原生谓词

(define (test-even? x)
  (even? x)
) ;define

(define (test-odd? x)
  (odd? x)
) ;define

(check (vector-par-filter test-even? #(1 2 3 4 5 6)) => #(2 4 6))
(check (vector-par-filter test-odd? #(1 2 3 4 5 6)) => #(1 3 5))
(check (vector-par-filter even? #(1 2 3 4 5 6 7 8)) => #(2 4 6 8))

;; 3. 匿名 lambda 与复合条件
(check (vector-par-filter (lambda (x) (> x 10)) #(5 12 8 20 3)) => #(12 20))

;; 4. 全保留与全剔除
(check (vector-par-filter (lambda (x) #t) #(1 2 3)) => #(1 2 3))
(check (vector-par-filter (lambda (x) #f) #(1 2 3)) => #())

;; 5. 广义真值判定测试（除 #f 外均为真值）
(check
  (vector-par-filter (lambda (x) (if (even? x) x #f)) #(1 2 3 4))
  =>
  #(2 4)
) ;check
(check
  (vector-par-filter (lambda (x) (if (= x 1) 0 #f)) #(1 2 3))
  =>
  #(1)
) ;check
(check
  (vector-par-filter (lambda (x) (if (= x 2) '() #f)) #(1 2 3))
  =>
  #(2)
) ;check
(check
  (vector-par-filter (lambda (x) (if (= x 3) "yes" #f)) #(1 2 3))
  =>
  #(3)
) ;check

;; 6. 逆序耗时严格保序测试
;; 各元素模拟不同耗时（首元素耗时最长，尾元素耗时最短），验证即使完成乱序，结果仍然严格保序

(define order-res
  (vector-par-filter
    (lambda (x) (sleep (* (- 5 x) 0.02)) (even? x))
    #(1 2 3 4)
  ) ;vector-par-filter
) ;define
(check order-res => #(2 4))

;; 7. 闭包自由变量捕获测试

(define threshold-res
  (let ((threshold 15))
    (vector-par-filter (lambda (x) (> x threshold)) #(10 20 5 25 15))
  ) ;let
) ;define
(check threshold-res => #(20 25))

;; 8. 参数类型校验（type-error）
(check-catch 'type-error (vector-par-filter 123 #(1 2 3)))
(check-catch 'type-error (vector-par-filter "not-a-proc" #(1 2 3)))
(check-catch 'type-error (vector-par-filter (lambda (x) (even? x)) 123))
(check-catch 'type-error
  (vector-par-filter (lambda (x) (even? x)) "not-a-vector")
) ;check-catch
(check-catch 'type-error (vector-par-filter (lambda (x) (even? x)) '(1 2 3)))
(check-catch 'type-error (vector-par-filter display #(1 2 3)))

;; 9. 异常传播与孤儿任务防护测试
(check-catch 'pred-err
  (vector-par-filter (lambda (x) (if (= x 2) (error 'pred-err "pred-failed") (even? x)))
    #(1 2 3 4)
  ) ;vector-par-filter
) ;check-catch

;; 10. 字符串向量与复合条件过滤
(check
  (vector-par-filter (lambda (s) (char=? (string-ref s 0) #\a))
    #("apple" "banana" "avocado" "cherry")
  ) ;vector-par-filter
  =>
  #("apple" "avocado")
) ;check

;; 11. 并发加速效果验证
(when (>= (go-worker-count) 4)
  ;; 先热身，触发线程池与 worker 初始化，避免一次性启动开销计入计时窗口
  (vector-par-filter (lambda (x) #t) #(1))
  (let ((t0 (current-jiffy)))
    (define res (vector-par-filter (lambda (x) (sleep 0.1) (even? x)) #(1 2 3 4)))
    (let* ((dt (/ (- (current-jiffy) t0) (jiffies-per-second))) (ms (* dt 1000.0)))
      (check res => #(2 4))
      (check (< ms 350) => #t)
    ) ;let*
  ) ;let
) ;when

(check-report)
