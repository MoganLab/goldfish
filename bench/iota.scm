;; ; IOTA 函数性能基准测试
;; ; Copyright (C) 2026 The Goldfish Scheme Authors
;; ; 比较三个实现版本的性能差异

(import (scheme time)
  (liii base)
  (liii error)
) ;import

;; ; 旧版本 - make-list + do + set!
(define* (iota-old count (start 0) (step 1))
  (when (not (integer? count))
    (type-error "iota: count must be an integer"
    ) ;type-error
  ) ;when
  (when (< count 0)
    (value-error "iota: count must be positive but received ~d"
      count
    ) ;value-error
  ) ;when
  (let ((lst (make-list count)))
    (do ((p lst (cdr p)) (i start (+ i step)))
      ((null? p) lst)
      (set! (car p) i)
    ) ;do
  ) ;let
) ;define*

;; ; 有问题版本 - 尾递归 + reverse
(define* (iota-reverse count (start 0) (step 1))
  (when (not (integer? count))
    (type-error "iota: count must be an integer"
    ) ;type-error
  ) ;when
  (when (< count 0)
    (value-error "iota: count must be non-negative but received ~d"
      count
    ) ;value-error
  ) ;when
  (let loop
    ((i 0) (current start) (acc '()))
    (if (= i count)
      (reverse acc)
      (loop (+ i 1)
        (+ current step)
        (cons current acc)
      ) ;loop
    ) ;if
  ) ;let
) ;define*

;; ; 最新版本 - do 循环，避免 reverse
(define* (iota-new count (start 0) (step 1))
  (when (not (integer? count))
    (type-error "iota: count must be an integer"
    ) ;type-error
  ) ;when
  (when (< count 0)
    (value-error "iota: count must be non-negative but received ~d"
      count
    ) ;value-error
  ) ;when
  (do ((i count (- i 1))
       (val (+ start (* (- count 1) step))
         (- val step)
       ) ;val
       (result '() (cons val result))
      ) ;
    ((zero? i) result)
  ) ;do
) ;define*

;; ; 测试框架
(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy))
        ) ;
    (display msg)
    (display (number->string (- end start)))
    (display " jiffies\n")
  ) ;let*
) ;define

(define (repeat n proc)
  (when (> n 0)
    (proc)
    (repeat (- n 1) proc)
  ) ;when
) ;define

;; ; 验证正确性
(define (verify)
  (display "验证正确性...\n")
  (let ((test-cases '((10 0 1) (100 5 2) (1000 10 3))
        ) ;test-cases
       ) ;
    (for-each (lambda (case
                      ) ;case
                (let ((count (car case))
                      (start (cadr case))
                      (step (caddr case))
                     ) ;
                  (let ((result-old (iota-old count start step))
                        (result-reverse (iota-reverse count start step)
                        ) ;result-reverse
                        (result-new (iota-new count start step))
                        (result-c (iota count start step))
                       ) ;
                    (if (and (equal? result-old result-reverse)
                          (equal? result-old result-new)
                          (equal? result-old result-c)
                        ) ;and
                      (display (string-append "✓ iota("
                                 (number->string count)
                                 ", "
                                 (number->string start)
                                 ", "
                                 (number->string step)
                                 ") - 一致\n"
                               ) ;string-append
                      ) ;display
                      (display (string-append "✗ iota("
                                 (number->string count)
                                 ", "
                                 (number->string start)
                                 ", "
                                 (number->string step)
                                 ") - 不一致!\n"
                               ) ;string-append
                      ) ;display
                    ) ;if
                  ) ;let
                ) ;let
              ) ;lambda
      test-cases
    ) ;for-each
  ) ;let
) ;define

;; ; 性能测试
(define (run-benchmarks)
  (display "\n=== IOTA 性能测试 ===\n\n"
  ) ;display

  ;; 小列表测试 (1000 个元素, 1000 次)
  (display "小列表 (1000 个元素, 1000 次迭代):\n"
  ) ;display
  (timing "旧版本:\t\t"
    (lambda ()
      (repeat 1000
        (lambda () (iota-old 1000))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "reverse版本:\t"
    (lambda ()
      (repeat 1000
        (lambda () (iota-reverse 1000))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "新版本:\t\t"
    (lambda ()
      (repeat 1000
        (lambda () (iota-new 1000))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "c版本:\t\t"
    (lambda ()
      (repeat 1000 (lambda () (iota 1000)))
    ) ;lambda
  ) ;timing

  (display "\n")

  ;; 大列表测试 (10000 个元素, 100 次)
  (display "大列表 (10000 个元素, 100 次迭代):\n"
  ) ;display
  (timing "旧版本:\t\t"
    (lambda ()
      (repeat 100
        (lambda () (iota-old 10000))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "reverse版本:\t"
    (lambda ()
      (repeat 100
        (lambda () (iota-reverse 10000))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "新版本:\t\t"
    (lambda ()
      (repeat 100
        (lambda () (iota-new 10000))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "c版本:\t\t"
    (lambda ()
      (repeat 100 (lambda () (iota 10000)))
    ) ;lambda
  ) ;timing

  (display "\n")

  ;; 超大列表测试 (100000 个元素, 10 次)
  (display "超大列表 (100000 个元素, 10 次迭代):\n"
  ) ;display
  (timing "旧版本:\t\t"
    (lambda ()
      (repeat 10
        (lambda () (iota-old 100000))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "reverse版本:\t"
    (lambda ()
      (repeat 10
        (lambda () (iota-reverse 100000))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "新版本:\t\t"
    (lambda ()
      (repeat 10
        (lambda () (iota-new 100000))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "c版本:\t\t"
    (lambda ()
      (repeat 10 (lambda () (iota 100000)))
    ) ;lambda
  ) ;timing

  (display "\n")

  ;; 非标准参数测试 (5000 个元素, start=10, step=3, 200 次)
  (display "非标准参数 (5000 个元素, start=10, step=3, 200 次迭代):\n"
  ) ;display
  (timing "旧版本:\t\t"
    (lambda ()
      (repeat 200
        (lambda () (iota-old 5000 10 3))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "reverse版本:\t"
    (lambda ()
      (repeat 200
        (lambda () (iota-reverse 5000 10 3))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "新版本:\t\t"
    (lambda ()
      (repeat 200
        (lambda () (iota-new 5000 10 3))
      ) ;repeat
    ) ;lambda
  ) ;timing
  (timing "c版本:\t\t"
    (lambda ()
      (repeat 200
        (lambda () (iota 5000 10 3))
      ) ;repeat
    ) ;lambda
  ) ;timing
) ;define

;; ; 运行测试
(verify)
(run-benchmarks)
