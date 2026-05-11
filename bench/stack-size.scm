;; stack-size 性能基准测试
;; 测试当前 O(n) 实现的性能，为后续优化提供基准数据

(import (liii timeit)
        (liii stack)
        (scheme base)
)

;; 创建指定大小的栈
(define (make-stack-of-size n)
  (let loop ((i 0) (s (make-stack)))
    (if (= i n)
      s
      (loop (+ i 1) (stack-push! s i))
    )
  )
)

;; 运行单次 benchmark
(define (bench name stack-size-call number)
  (let ((elapsed (timeit stack-size-call '() number)))
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
  (display "=== stack-size 性能测试 ===\n\n")

  ;; 小栈 (10 个元素)
  (let ((small-stack (make-stack-of-size 10)))
    (bench "小栈 (10 元素)  "
           (lambda () (stack-size small-stack))
           1000000)
  )

  ;; 中栈 (100 个元素)
  (let ((medium-stack (make-stack-of-size 100)))
    (bench "中栈 (100 元素) "
           (lambda () (stack-size medium-stack))
           1000000)
  )

  ;; 大栈 (1000 个元素)
  (let ((large-stack (make-stack-of-size 1000)))
    (bench "大栈 (1000 元素)"
           (lambda () (stack-size large-stack))
           1000000)
  )

  ;; 超大栈 (10000 个元素)
  (let ((xlarge-stack (make-stack-of-size 10000)))
    (bench "超大栈 (10000)  "
           (lambda () (stack-size xlarge-stack))
           100000)
  )

  ;; 极大栈 (100000 个元素)
  (let ((xxlarge-stack (make-stack-of-size 100000)))
    (bench "极大栈 (100000) "
           (lambda () (stack-size xxlarge-stack))
           10000)
  )
)

(run-benchmarks)
