;; list-drop 性能基准测试
;; 比较旧版本（两次遍历）和新版本（单次遍历）的性能差异

(import (scheme time)
  (scheme base)
  (srfi srfi-1)
  (liii error)
)

;; 旧版本 - 先调用 length 再调用 drop，两次遍历
(define (list-drop-old lst n)
  (unless (list? lst)
    (type-error "list-drop: first argument must be a list" lst)
  )
  (unless (integer? n)
    (type-error "list-drop: second argument must be an integer" n)
  )
  (cond ((< n 0) lst)
        ((>= n (length lst)) '())
        (else (drop lst n))
  )
)

;; 新版本 - 单次遍历，直接返回剩余部分
(define (list-drop-new lst n)
  (unless (list? lst)
    (type-error "list-drop: first argument must be a list" lst)
  )
  (unless (integer? n)
    (type-error "list-drop: second argument must be an integer" n)
  )
  (cond ((< n 0) lst)
        ((= n 0) lst)
        (else (let loop
                ((rest lst) (count 0))
                (cond ((null? rest) '())
                      ((>= count n) rest)
                      (else (loop (cdr rest) (+ count 1)))
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
    (let ((old-result (list-drop-old test-list 5))
          (new-result (list-drop-new test-list 5))
         )
      (if (equal? old-result new-result)
        (display "✓ list-drop(list, 5) - 一致\n")
        (display "✗ list-drop(list, 5) - 不一致!\n")
      )
    )
    ;; 全部丢弃
    (let ((old-result (list-drop-old test-list 100))
          (new-result (list-drop-new test-list 100))
         )
      (if (equal? old-result new-result)
        (display "✓ list-drop(list, 100) - 一致\n")
        (display "✗ list-drop(list, 100) - 不一致!\n")
      )
    )
    ;; n = 0
    (let ((old-result (list-drop-old test-list 0))
          (new-result (list-drop-new test-list 0))
         )
      (if (equal? old-result new-result)
        (display "✓ list-drop(list, 0) - 一致\n")
        (display "✗ list-drop(list, 0) - 不一致!\n")
      )
    )
    ;; n < 0
    (let ((old-result (list-drop-old test-list -1))
          (new-result (list-drop-new test-list -1))
         )
      (if (equal? old-result new-result)
        (display "✓ list-drop(list, -1) - 一致\n")
        (display "✗ list-drop(list, -1) - 不一致!\n")
      )
    )
    ;; 空列表
    (let ((old-result (list-drop-old empty-list 5))
          (new-result (list-drop-new empty-list 5))
         )
      (if (equal? old-result new-result)
        (display "✓ list-drop(empty, 5) - 一致\n")
        (display "✗ list-drop(empty, 5) - 不一致!\n")
      )
    )
  )
)

;; 性能测试
(define (run-benchmarks)
  (display "\n=== list-drop 性能测试 ===\n\n")

  ;; 小列表测试 (100 个元素, 10000 次)
  (display "小列表 (100 个元素, 丢弃 10 个, 10000 次迭代):\n")
  (let ((small-list (iota 100)))
    (timing "旧版本:\t\t"
      (lambda () (repeat 10000 (lambda () (list-drop-old small-list 10))))
    )
    (timing "新版本:\t\t"
      (lambda () (repeat 10000 (lambda () (list-drop-new small-list 10))))
    )
  )

  (display "\n")

  ;; 中等列表测试 (1000 个元素, 丢弃 100 个, 5000 次)
  (display "中等列表 (1000 个元素, 丢弃 100 个, 5000 次迭代):\n")
  (let ((medium-list (iota 1000)))
    (timing "旧版本:\t\t"
      (lambda () (repeat 5000 (lambda () (list-drop-old medium-list 100))))
    )
    (timing "新版本:\t\t"
      (lambda () (repeat 5000 (lambda () (list-drop-new medium-list 100))))
    )
  )

  (display "\n")

  ;; 大列表丢弃少量 (10000 个元素, 丢弃 10 个, 1000 次)
  (display "大列表丢弃少量 (10000 个元素, 丢弃 10 个, 1000 次迭代):\n")
  (let ((large-list (iota 10000)))
    (timing "旧版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-drop-old large-list 10))))
    )
    (timing "新版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-drop-new large-list 10))))
    )
  )

  (display "\n")

  ;; 大列表丢弃一半 (10000 个元素, 丢弃 5000 个, 1000 次)
  (display "大列表丢弃一半 (10000 个元素, 丢弃 5000 个, 1000 次迭代):\n")
  (let ((large-list (iota 10000)))
    (timing "旧版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-drop-old large-list 5000))))
    )
    (timing "新版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-drop-new large-list 5000))))
    )
  )

  (display "\n")

  ;; 丢弃全部元素
  (display "丢弃全部元素 (10000 个元素, 丢弃 10000 个, 1000 次迭代):\n")
  (let ((large-list (iota 10000)))
    (timing "旧版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-drop-old large-list 10000))))
    )
    (timing "新版本:\t\t"
      (lambda () (repeat 1000 (lambda () (list-drop-new large-list 10000))))
    )
  )
)

;; 运行测试
(verify)
(run-benchmarks)
