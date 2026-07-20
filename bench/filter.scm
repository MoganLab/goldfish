;; filter 性能基准测试
;; 测试 (liii list) 中 filter 的性能，为 C 实现（s7_liii_list.c）提供基准数据

(import (liii timeit) (liii list) (scheme base))

;; 运行单次 benchmark

(define (bench name stmt number)
  (let ((elapsed (timeit stmt '() number)))
    (display name)
    (display ": ")
    (display elapsed)
    (display " 秒 (")
    (display number)
    (display " 次)\n")
  ) ;let
) ;define

;; 性能测试

(define (run-benchmarks)
  (display "=== filter 性能测试 ===\n\n")

  ;; 空列表
  (let ((empty '()))
    (bench "空列表            "
      (lambda () (filter (lambda (x) #t) empty))
      100000
    ) ;bench
  ) ;let

  ;; 小列表 (10 元素)，全部匹配
  (let ((small (iota 10)))
    (bench "小列表(10)全匹配  "
      (lambda () (filter (lambda (x) #t) small))
      100000
    ) ;bench
  ) ;let

  ;; 小列表 (10 元素)，匹配一半
  (let ((small (iota 10)))
    (bench "小列表(10)半匹配  " (lambda () (filter even? small)) 100000)
  ) ;let

  ;; 小列表 (10 元素)，全不匹配
  (let ((small (iota 10)))
    (bench "小列表(10)全不匹配"
      (lambda () (filter (lambda (x) #f) small))
      100000
    ) ;bench
  ) ;let

  ;; 中列表 (100 元素)，匹配一半
  (let ((medium (iota 100)))
    (bench "中列表(100)半匹配 " (lambda () (filter even? medium)) 100000)
  ) ;let

  ;; 大列表 (1000 元素)，匹配一半
  (let ((large (iota 1000)))
    (bench "大列表(1000)半匹配" (lambda () (filter even? large)) 10000)
  ) ;let

  ;; 超大列表 (10000 元素)，匹配一半
  (let ((xlarge (iota 10000)))
    (bench "超大列表(10000)   " (lambda () (filter even? xlarge)) 1000)
  ) ;let

  ;; 前半不匹配后半全匹配 (10000 元素)：最能体现结构共享的场景
  (let ((l (iota 10000)))
    (bench "后半全匹配(10000) "
      (lambda () (filter (lambda (x) (>= x 5000)) l))
      1000
    ) ;bench
  ) ;let

  ;; 前半全匹配后半不匹配 (10000 元素)：无法共享结构，全部重新 cons
  (let ((l (iota 10000)))
    (bench "前半全匹配(10000) "
      (lambda () (filter (lambda (x) (< x 5000)) l))
      1000
    ) ;bench
  ) ;let

  ;; 直调 rootlet 的 g_filter：验证 (define filter g_filter) 别名没有额外开销
  (let ((medium (iota 100)))
    (bench "g_filter直调(100) " (lambda () (g_filter even? medium)) 100000)
  ) ;let
  (let ((l (iota 10000)))
    (bench "g_filter直调10000 "
      (lambda () (g_filter (lambda (x) (< x 5000)) l))
      1000
    ) ;bench
  ) ;let
) ;define

(run-benchmarks)
