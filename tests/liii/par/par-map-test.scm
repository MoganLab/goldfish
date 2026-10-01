(import (liii check) (liii par) (liii go) (liii time) (scheme time))

(check-set-mode! 'report-failed)

;; par-map
;;
;; 语法
;; ----
;; (par-map f l)
;;
;; 参数
;; ----
;; f : procedure
;; 并发映射的过程，接受列表的一个元素作为参数，返回映射后的结果。
;; f 会在后台 worker 线程的独立环境中运行，其参数与返回值经序列化传输。
;;
;; l : list
;; 待映射的列表，列表元素须为可序列化的数据。
;;
;; 返回值
;; ----
;; list
;; 与输入列表等长且严格保序的映射结果列表。
;;
;; 说明
;; ----
;; 1. 基于 (liii go) 线程池并发执行，严格保证返回结果列表的顺序与原列表一致。
;; 2. 若 l 为空列表，直接返回 '()，不启动后台任务。
;; 3. 主线程会等待所有 worker 执行完毕后才返回。
;; 4. 若后台 worker 发生未捕获异常，主线程在收敛全部任务后重新抛出最早捕获的异常。
;; 5. 若参数类型不匹配，抛出 type-error。

;; 1. 空列表测试
(check (par-map (lambda (x) (* x x)) '()) => '())

;; 2. 使用命名过程并发保序映射

(define (test-square x)
  (* x x)
) ;define

(check (par-map test-square '(1 2 3 4 5)) => '(1 4 9 16 25))

;; 3. 使用匿名 lambda 并发保序映射
(check (par-map (lambda (x) (+ x 100)) '(10 20 30)) => '(110 120 130))

;; 4. 逆序耗时严格保序测试
;; 各元素模拟不同耗时（首元素耗时最长，尾元素耗时最短），验证即使完成乱序，结果仍然严格保序

(define order-res
  (par-map
    (lambda (x) (sleep (* (- 5 x) 0.02)) (* x x))
    '(1 2 3 4)
  ) ;par-map
) ;define
(check order-res => '(1 4 9 16))

;; 5. 闭包自由变量捕获测试

(define factor-res
  (let ((factor 10))
    (par-map (lambda (x) (* x factor)) '(1 2 3))
  ) ;let
) ;define
(check factor-res => '(10 20 30))

;; 6. 参数类型校验（type-error）
(check-catch 'type-error (par-map 123 '(1 2 3)))
(check-catch 'type-error (par-map "not-a-proc" '(1 2 3)))
(check-catch 'type-error (par-map (lambda (x) x) 123))
(check-catch 'type-error (par-map (lambda (x) x) "not-a-list"))
(check-catch 'type-error (par-map (lambda (x) x) #(1 2 3)))
;; display 是合法过程，但其返回的未定义值无法序列化，worker 端抛出 type-error
(check-catch 'type-error (par-map display '(1 2 3)))

;; 7. 异常传播与孤儿任务防护测试
(check-catch 'worker-err
  (par-map (lambda (x) (if (= x 2) (error 'worker-err "worker-failed") (* x 10)))
    '(1 2 3)
  ) ;par-map
) ;check-catch

;; 8. 并发加速效果验证
(when (>= (go-worker-count) 4)
  ;; 先热身，触发线程池与 worker 初始化，避免一次性启动开销计入计时窗口
  (par-map (lambda (x) x) '(1))
  (let ((t0 (current-jiffy)))
    (define res (par-map (lambda (x) (sleep 0.1) (* x 2)) '(1 2 3 4)))
    (let* ((dt (/ (- (current-jiffy) t0) (jiffies-per-second))) (ms (* dt 1000.0)))
      (check res => '(2 4 6 8))
      (check (< ms 350) => #t)
    ) ;let*
  ) ;let
) ;when

(check-report)
