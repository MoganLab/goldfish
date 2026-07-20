;; drop 性能基准测试
;; drop 在 srfi-1 中定义为 list-tail 的别名，而 list-tail 是带 p_pp 快速路径的
;; C 内置函数。本基准用于验证 drop 是否已有 C 级性能，以及别名是否有额外开销。

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
  (display "=== drop 性能测试 ===\n\n")

  (let ((small (iota 10)))
    (bench "小列表(10) drop 5    " (lambda () (drop small 5)) 100000)
  ) ;let

  (let ((medium (iota 100)))
    (bench "中列表(100) drop 50  " (lambda () (drop medium 50)) 100000)
  ) ;let

  (let ((large (iota 1000)))
    (bench "大列表(1000) drop 500" (lambda () (drop large 500)) 10000)
  ) ;let

  (let ((xlarge (iota 10000)))
    (bench "超大列表(10000) 5000 " (lambda () (drop xlarge 5000)) 1000)
  ) ;let

  ;; 直调 list-tail：验证 (define drop list-tail) 别名没有额外开销
  (let ((medium (iota 100)))
    (bench "list-tail直调(100)   " (lambda () (list-tail medium 50)) 100000)
  ) ;let
  (let ((xlarge (iota 10000)))
    (bench "list-tail直调(10000) " (lambda () (list-tail xlarge 5000)) 1000)
  ) ;let
) ;define

(run-benchmarks)
