;; every 性能基准测试
;; 测试 (srfi srfi-1) / (liii list) 中 every 的性能

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
  (display "=== every 性能测试 ===\n\n")

  (bench "短列表(3元素) 首元素即不满足 "
    (lambda () (every even? '(1 2 3)))
    100000
  ) ;bench

  (bench "短列表(3元素) 全部满足      "
    (lambda () (every odd? '(1 3 5)))
    100000
  ) ;bench

  (let ((mid-list (iota 100)))
    (bench "中列表(100元素) 末尾不满足   "
      (lambda () (every (lambda (x) (< x 99)) mid-list))
      10000
    ) ;bench

    (bench "中列表(100元素) 全部满足    "
      (lambda () (every (lambda (x) (< x 100)) mid-list))
      10000
    ) ;bench
  ) ;let

  (let ((long-list (iota 1000)))
    (bench "长列表(1000元素) 末尾不满足  "
      (lambda () (every (lambda (x) (< x 999)) long-list))
      1000
    ) ;bench

    (bench "长列表(1000元素) 全部满足   "
      (lambda () (every (lambda (x) (< x 1000)) long-list))
      1000
    ) ;bench
  ) ;let

  (let ((long-list (iota 10000)))
    (bench "超长列表(10000元素) 全部满足"
      (lambda () (every (lambda (x) (< x 10000)) long-list))
      100
    ) ;bench
  ) ;let
) ;define

(run-benchmarks)
