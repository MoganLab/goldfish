;; take 性能基准测试
;; 测试 (liii list) 中 take 的性能，为 C 实现（s7_liii_list.c）提供基准数据

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
  (display "=== take 性能测试 ===\n\n")

  ;; 空列表
  (let ((empty '()))
    (bench "空列表 take 0      " (lambda () (take empty 0)) 100000)
  ) ;let

  ;; 小列表 (10 元素)，取前 0 个
  (let ((small (iota 10)))
    (bench "小列表(10) take 0  " (lambda () (take small 0)) 100000)
  ) ;let

  ;; 小列表 (10 元素)，取前 5 个
  (let ((small (iota 10)))
    (bench "小列表(10) take 5  " (lambda () (take small 5)) 100000)
  ) ;let

  ;; 小列表 (10 元素)，全部取走
  (let ((small (iota 10)))
    (bench "小列表(10) take 10 " (lambda () (take small 10)) 100000)
  ) ;let

  ;; 中列表 (100 元素)，取一半
  (let ((medium (iota 100)))
    (bench "中列表(100) take 50" (lambda () (take medium 50)) 100000)
  ) ;let

  ;; 大列表 (1000 元素)，取一半
  (let ((large (iota 1000)))
    (bench "大列表(1000) 500   " (lambda () (take large 500)) 10000)
  ) ;let

  ;; 超大列表 (10000 元素)，取一半
  (let ((xlarge (iota 10000)))
    (bench "超大列表(10000)    " (lambda () (take xlarge 5000)) 1000)
  ) ;let

  ;; 超大列表 (10000 元素)，全部取走
  (let ((xlarge (iota 10000)))
    (bench "全取(10000)        " (lambda () (take xlarge 10000)) 1000)
  ) ;let

  ;; 直调 rootlet 的 g_take：验证 (define take g_take) 别名没有额外开销
  (let ((medium (iota 100)))
    (bench "g_take直调(100)    " (lambda () (g_take medium 50)) 100000)
  ) ;let
  (let ((xlarge (iota 10000)))
    (bench "g_take直调(10000)  " (lambda () (g_take xlarge 5000)) 1000)
  ) ;let
) ;define

(run-benchmarks)
