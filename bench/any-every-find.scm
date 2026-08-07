;; any / every / find 性能基准测试
;; 测试 (liii list) 中 any / every / find 的性能，为 C 实现（s7_liii_list.c）提供基准数据

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
  (display "=== any / every / find 性能测试 ===\n\n")

  ;; 空列表
  (let ((empty '()))
    (bench "any 空列表              "
      (lambda () (any (lambda (x) #t) empty))
      100000
    ) ;bench
    (bench "every 空列表            "
      (lambda () (every (lambda (x) #t) empty))
      100000
    ) ;bench
    (bench "find 空列表             "
      (lambda () (find (lambda (x) #t) empty))
      100000
    ) ;bench
  ) ;let

  ;; 小列表 (10 元素)
  (let ((small (iota 10)))
    (bench "any 小列表(10)首部命中  "
      (lambda () (any (lambda (x) (= x 0)) small))
      100000
    ) ;bench
    (bench "any 小列表(10)未命中    "
      (lambda () (any (lambda (x) (= x 100)) small))
      100000
    ) ;bench
    (bench "every 小列表(10)全通过  "
      (lambda () (every (lambda (x) (< x 10)) small))
      100000
    ) ;bench
    (bench "every 小列表(10)首部失败"
      (lambda () (every (lambda (x) (< x 0)) small))
      100000
    ) ;bench
    (bench "find 小列表(10)尾部命中 "
      (lambda () (find (lambda (x) (= x 9)) small))
      100000
    ) ;bench
  ) ;let

  ;; 中列表 (100 元素)
  (let ((medium (iota 100)))
    (bench "any 中列表(100)未命中   "
      (lambda () (any (lambda (x) (= x 100)) medium))
      100000
    ) ;bench
    (bench "every 中列表(100)全通过 "
      (lambda () (every (lambda (x) (< x 100)) medium))
      100000
    ) ;bench
    (bench "find 中列表(100)尾部命中"
      (lambda () (find (lambda (x) (= x 99)) medium))
      100000
    ) ;bench
  ) ;let

  ;; 大列表 (1000 元素)
  (let ((large (iota 1000)))
    (bench "any 大列表(1000)未命中  "
      (lambda () (any (lambda (x) (= x 1000)) large))
      10000
    ) ;bench
    (bench "every 大列表(1000)全通过"
      (lambda () (every (lambda (x) (< x 1000)) large))
      10000
    ) ;bench
    (bench "find 大列表(1000)未命中 "
      (lambda () (find (lambda (x) (= x 1000)) large))
      10000
    ) ;bench
  ) ;let
) ;define

(run-benchmarks)
