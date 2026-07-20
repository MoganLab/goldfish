;; take-right / drop-right 性能基准测试
;; 测试 (liii list) 中 take-right 和 drop-right 的性能，为 C 实现提供基准数据

(import (liii timeit) (liii list) (scheme base))

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

(define (run-benchmarks)
  (display "=== take-right / drop-right 性能测试 ===\n\n")

  (let ((small (iota 10)))
    (bench "小列表(10) take-right 5   " (lambda () (take-right small 5)) 100000)
  ) ;let
  (let ((small (iota 10)))
    (bench "小列表(10) drop-right 5   " (lambda () (drop-right small 5)) 100000)
  ) ;let

  (let ((medium (iota 100)))
    (bench "中列表(100) take-right 50 "
      (lambda () (take-right medium 50))
      100000
    ) ;bench
  ) ;let
  (let ((medium (iota 100)))
    (bench "中列表(100) drop-right 50 "
      (lambda () (drop-right medium 50))
      100000
    ) ;bench
  ) ;let

  (let ((large (iota 1000)))
    (bench "大列表(1000) take-right   " (lambda () (take-right large 500)) 10000)
  ) ;let
  (let ((large (iota 1000)))
    (bench "大列表(1000) drop-right   " (lambda () (drop-right large 500)) 10000)
  ) ;let

  (let ((xlarge (iota 10000)))
    (bench "超大列表(10000) take-right"
      (lambda () (take-right xlarge 5000))
      1000
    ) ;bench
  ) ;let
  (let ((xlarge (iota 10000)))
    (bench "超大列表(10000) drop-right"
      (lambda () (drop-right xlarge 5000))
      1000
    ) ;bench
  ) ;let

  ;; 直调 rootlet 的 g_take_right / g_drop_right：验证别名没有额外开销
  (when (defined? 'g_take_right)
    (let ((medium (iota 100)))
      (bench "g_take_right直调(100)     "
        (lambda () (g_take_right medium 50))
        100000
      ) ;bench
    ) ;let
    (let ((medium (iota 100)))
      (bench "g_drop_right直调(100)     "
        (lambda () (g_drop_right medium 50))
        100000
      ) ;bench
    ) ;let
  ) ;when
) ;define

(run-benchmarks)
