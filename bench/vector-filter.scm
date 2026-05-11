;; vector-filter 性能基准测试
;; 测试当前实现的性能，为后续优化提供基准数据

(import (liii timeit)
        (liii vector)
        (scheme base)
)

;; 运行单次 benchmark
(define (bench name stmt number)
  (let ((elapsed (timeit stmt '() number)))
    (display name)
    (display ": ")
    (display elapsed)
    (display " 秒 (")
    (display number)
    (display " 次)\n")
  )
)

;; 性能测试
(define (run-benchmarks)
  (display "=== vector-filter 性能测试 ===\n\n")

  ;; 空向量
  (let ((empty #( )))
    (bench "空向量          "
           (lambda () (vector-filter (lambda (x) #t) empty))
           100000)
  )

  ;; 小向量 (10 元素)，全部匹配
  (let ((small #(1 2 3 4 5 6 7 8 9 10)))
    (bench "小向量(10)全匹配"
           (lambda () (vector-filter (lambda (x) #t) small))
           100000)
  )

  ;; 小向量 (10 元素)，匹配一半
  (let ((small #(1 2 3 4 5 6 7 8 9 10)))
    (bench "小向量(10)半匹配"
           (lambda () (vector-filter even? small))
           100000)
  )

  ;; 中向量 (100 元素)，匹配一半
  (let ((medium (list->vector (iota 100))))
    (bench "中向量(100)半匹配"
           (lambda () (vector-filter even? medium))
           100000)
  )

  ;; 大向量 (1000 元素)，匹配一半
  (let ((large (list->vector (iota 1000))))
    (bench "大向量(1000)半匹配"
           (lambda () (vector-filter even? large))
           10000)
  )

  ;; 超大向量 (10000 元素)，匹配一半
  (let ((xlarge (list->vector (iota 10000))))
    (bench "超大向量(10000)  "
           (lambda () (vector-filter even? xlarge))
           1000))
  )

(run-benchmarks)
