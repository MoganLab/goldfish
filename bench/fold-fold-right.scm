;; fold / fold-right 性能基准测试
;; 测试 (liii list) 中 fold / fold-right 单列表路径的性能，
;; 为 C 实现（s7_liii_list.c 的 g_fold / g_fold_right）提供基准数据

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
  (display "=== fold / fold-right 性能测试 ===\n\n")

  ;; 空列表
  (let ((empty '()))
    (bench "fold 空列表                "
      (lambda () (fold + 0 empty))
      100000
    ) ;bench
    (bench "fold-right 空列表          "
      (lambda () (fold-right + 0 empty))
      100000
    ) ;bench
  ) ;let

  ;; 小列表 (10 元素)
  (let ((small (iota 10)))
    (bench "fold 小列表(10)求和        "
      (lambda () (fold + 0 small))
      100000
    ) ;bench
    (bench "fold-right 小列表(10)求和  "
      (lambda () (fold-right + 0 small))
      100000
    ) ;bench
    (bench "fold 小列表(10)cons       "
      (lambda () (fold cons '() small))
      100000
    ) ;bench
    (bench "fold-right 小列表(10)cons "
      (lambda () (fold-right cons '() small))
      100000
    ) ;bench
  ) ;let

  ;; 中列表 (100 元素)
  (let ((medium (iota 100)))
    (bench "fold 中列表(100)求和       "
      (lambda () (fold + 0 medium))
      100000
    ) ;bench
    (bench "fold-right 中列表(100)求和 "
      (lambda () (fold-right + 0 medium))
      100000
    ) ;bench
  ) ;let

  ;; 大列表 (1000 元素)
  (let ((large (iota 1000)))
    (bench "fold 大列表(1000)求和      "
      (lambda () (fold + 0 large))
      10000
    ) ;bench
    (bench "fold-right 大列表(1000)求和"
      (lambda () (fold-right + 0 large))
      10000
    ) ;bench
  ) ;let

  ;; 超大列表 (10000 元素)：fold-right 的 Scheme 实现非尾递归，顺带压栈
  (let ((huge (iota 10000)))
    (bench "fold 超大列表(10000)求和   "
      (lambda () (fold + 0 huge))
      1000
    ) ;bench
    (bench "fold-right 超大列表(10000) "
      (lambda () (fold-right + 0 huge))
      1000
    ) ;bench
  ) ;let
) ;define

(run-benchmarks)
