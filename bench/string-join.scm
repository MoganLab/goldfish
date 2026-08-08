;; string-join 性能基准测试
;; 测试 (srfi srfi-13) / (liii string) 中 string-join 的性能

(import (liii timeit) (liii string) (scheme base))

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
  (display "=== string-join 性能测试 ===\n\n")

  (bench "短列表(3元素) 默认分隔符   "
    (lambda () (string-join '("a" "b" "c")))
    100000
  ) ;bench

  (bench "短列表(3元素) 指定分隔符   "
    (lambda () (string-join '("a" "b" "c") ":"))
    100000
  ) ;bench

  (let ((mid-list (map number->string (iota 100))))
    (bench "中列表(100元素) 逗号分隔   "
      (lambda () (string-join mid-list ","))
      1000
    ) ;bench
  ) ;let

  (let ((long-list (map number->string (iota 1000))))
    (bench "长列表(1000元素) 逗号分隔  "
      (lambda () (string-join long-list ","))
      100
    ) ;bench
  ) ;let

  (let ((long-list (map number->string (iota 10000))))
    (bench "超长列表(10000元素) 逗号分隔"
      (lambda () (string-join long-list ","))
      10
    ) ;bench
  ) ;let
) ;define

(run-benchmarks)
