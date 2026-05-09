;; list-take 性能基准测试
;; 比较旧版本（两次遍历）和新版本（单次遍历）的性能差异

(import (scheme time)
  (scheme base)
  (srfi srfi-1)
  (liii error)
)

;; 旧版本 - 先调用 length 再调用 take，两次遍历
(define (list-take-old lst n)
  (unless (list? lst)
    (type-error "list-take: first argument must be a list" lst)
  )
  (unless (integer? n)
    (type-error "list-take: second argument must be an integer" n)
  )
  (cond ((< n 0) '())
        ((>= n (length lst)) lst)
        (else (take lst n))
  )
)

;; 新版本 - 单次遍历，边遍历边收集结果
(define (list-take-new lst n)
  (unless (list? lst)
    (type-error "list-take: first argument must be a list" lst)
  )
  (unless (integer? n)
    (type-error "list-take: second argument must be an integer" n)
  )
  (cond ((< n 0) '())
        ((= n 0) '())
        (else (let loop
                ((rest lst) (count 0) (result '()))
                (cond ((null? rest) lst)
                      ((>= count n) (reverse result))
                      (else (loop (cdr rest) (+ count 1) (cons (car rest) result)))
                )
              )
        )
  )
)

;; 测试框架
(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy))
        )
    (display msg)
    (display (number->string (- end start)))
    (display " jiffies\n")
  )
)

(define (repeat n proc)
  (when (> n 0)
    (proc)
    (repeat (- n 1) proc)
  )
)

;; 验证正确性
(define (verify)
  (display "验证正确性...\n")
  (let ((test-list '(1 2 3 4 5 6 7 8 9 10))
        (empty-list '())
       )
    ;; 正常取值
    (let ((old-result (list-take-old test-list 5))
          (new-result (list-take-new test-list 5))
         )
      (if (equal? old-result new-result)
        (display "✓ list-take(list, 5) - 一致\n")
        (display "✗ list-take(list, 5) - 不一致!\n")
      )
    )
    ;; 取全部
    (let ((old-result (list-take-old test-list 100))
          (new-result (list-take-new test-list 100))
         )
      (if (equal? old-result new-result)
        (display "✓ list-take(list, 100) - 一致\n")
        (display "✗ list-take(list, 100) - 不一致!\n")
      )
    )
    ;; n = 0
    (let ((old-result (list-take-old test-list 0))
          (new-result (list-take-new test-list 0))
         )
      (if (equal? old-result new-result)
        (display "✓ list-take(list, 0) - 一致\n")
        (display "✗ list-take(list, 0) - 不一致!\n")
      )
    )
    ;; n < 0
    (let ((old-result (list-take-old test-list -1))
          (new-result (list-take-new test-list -1))
         )
      (if (equal? old-result new-result)
        (display "✓ list-take(list, -1) - 一致\n")
        (display "✗ list-take(list, -1) - 不一致!\n")
      )
    )
    ;; 空列表
    (let ((old-result (list-take-old empty-list 5))
          (new-result (list-take-new empty-list 5))
         )
      (if (equal? old-result new-result)
        (display "✓ list-take(empty, 5) - 一致\n")
        (display "✗ list-take(empty, 5) - 不一致!\n")
      )
    )
  )
)

;; 性能测试
(define (run-benchmarks)
  (display "\n=== list-take 性能测试 ===\n\n")

  ;; 小列表测试 (100 个元素, 10000 次)
  (display "小列表 (100 个元素, 取 10 个, 10000 次迭代):\n")
  (let ((small-list (iota 100)))
    (timing "旧版本:\t\t"
      (lambda () (repeat 10000 (lambda () (list-take-old small-list 10))))
    )
    (timing "新版本:\t\t"
      (lambda () (repeat 10000 (lambda () (list-take-new small-list 10))))
    )
  )

  (display "\n")

  ;; 中等列表测试 (1000 个元素, 取 100 个, 5000 次)
  (display "中等列表 (1000 个元素, 取 100 个, 5000 次迭代):\n")
  (let ((medium-list (iota 1000)))
    (timing "旧版本:\t\t"
      (lambda () (repeat 5000 (lambda () (list-take-old medium-list 100))))
    )
    (timing "新版本:\t\t"
      (lambda () (repeat 5000 (lambda () (list-take-new medium-list 100))))
    )
  )

  (display "\n")

  ;; 大列表取少量元素 (10000 个元素, 取 10 个, 1000 次)
  ;; 这是最能体现优化价值的场景：避免 length 遍历全部 10000 个元素
  (display "大列表取少量 (10000 个元素, 取 10 个, 1000 次迭代):\n")
  (let ((large-list (iota 10000)))
    (timing "旧版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-take-old large-list 10))))
    )
    (timing "新版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-take-new large-list 10))))
    )
  )

  (display "\n")

  ;; 大列表取一半 (10000 个元素, 取 5000 个, 1000 次)
  (display "大列表取一半 (10000 个元素, 取 5000 个, 1000 次迭代):\n")
  (let ((large-list (iota 10000)))
    (timing "旧版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-take-old large-list 5000))))
    )
    (timing "新版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-take-new large-list 5000))))
    )
  )

  (display "\n")

  ;; 取全部元素
  (display "取全部元素 (10000 个元素, 取 10000 个, 1000 次迭代):\n")
  (let ((large-list (iota 10000)))
    (timing "旧版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-take-old large-list 10000))))
    )
    (timing "新版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-take-new large-list 10000))))
    )
  )
)

;; 运行测试
(verify)
(run-benchmarks)
