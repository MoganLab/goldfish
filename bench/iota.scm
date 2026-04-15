;;; IOTA 函数性能基准测试
;;; Copyright (C) 2026 The Goldfish Scheme Authors
;;; 比较三个实现版本的性能差异

(import (scheme time)
        (liii base)
        (liii error))

;;; 旧版本 - make-list + do + set!
(define* (iota-old count (start 0) (step 1))
  (when (not (integer? count))
    (type-error "iota: count must be an integer"))
  (when (< count 0)
    (value-error "iota: count must be positive but received ~d" count))
  (let ((lst (make-list count)))
    (do ((p lst (cdr p))
         (i start (+ i step)))
      ((null? p) lst)
      (set! (car p) i))))

;;; 有问题版本 - 尾递归 + reverse
(define* (iota-reverse count (start 0) (step 1))
  (when (not (integer? count))
    (type-error "iota: count must be an integer"))
  (when (< count 0)
    (value-error "iota: count must be non-negative but received ~d" count))
  (let loop ((i 0) (current start) (acc '()))
    (if (= i count)
        (reverse acc)
        (loop (+ i 1) (+ current step) (cons current acc)))))

;;; 最新版本 - do 循环，避免 reverse
(define* (iota-new count (start 0) (step 1))
  (when (not (integer? count))
    (type-error "iota: count must be an integer"))
  (when (< count 0)
    (value-error "iota: count must be non-negative but received ~d" count))
  (do ((i count (- i 1))
       (val (+ start (* (- count 1) step)) (- val step))
       (result '() (cons val result)))
      ((zero? i) result)))

;;; 测试框架
(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (display msg)
    (display (number->string (- end start)))
    (display " jiffies\n")))

(define (repeat n proc)
  (when (> n 0)
        (proc)
        (repeat (- n 1) proc)))

;;; 验证正确性
(define (verify)
  (display "验证正确性...\n")
  (let ((test-cases '((10 0 1) (100 5 2) (1000 10 3))))
    (for-each
      (lambda (case)
        (let ((count (car case))
              (start (cadr case))
              (step (caddr case)))
          (let ((result-old (iota-old count start step))
                (result-reverse (iota-reverse count start step))
                (result-new (iota-new count start step))
                (result-c (iota count start step)))
            (if (and (equal? result-old result-reverse)
                     (equal? result-old result-new)
                     (equal? result-old result-c))
                (display (string-append "✓ iota(" (number->string count) ", " 
                                       (number->string start) ", " 
                                       (number->string step) ") - 一致\n"))
                (display (string-append "✗ iota(" (number->string count) ", " 
                                       (number->string start) ", " 
                                       (number->string step) ") - 不一致!\n"))))))
      test-cases)))

;;; 性能测试
(define (run-benchmarks)
  (display "\n=== IOTA 性能测试 ===\n\n")
  
  ;; 小列表测试 (1000 个元素, 1000 次)
  (display "小列表 (1000 个元素, 1000 次迭代):\n")
  (timing "旧版本:\t\t" 
    (lambda () (repeat 1000 (lambda () (iota-old 1000)))))
  (timing "reverse版本:\t" 
    (lambda () (repeat 1000 (lambda () (iota-reverse 1000)))))
  (timing "新版本:\t\t" 
    (lambda () (repeat 1000 (lambda () (iota-new 1000)))))
  (timing "c版本:\t\t"
    (lambda () (repeat 1000 (lambda () (iota 1000)))))
  
  (display "\n")
  
  ;; 大列表测试 (10000 个元素, 100 次)
  (display "大列表 (10000 个元素, 100 次迭代):\n")
  (timing "旧版本:\t\t" 
    (lambda () (repeat 100 (lambda () (iota-old 10000)))))
  (timing "reverse版本:\t" 
    (lambda () (repeat 100 (lambda () (iota-reverse 10000)))))
  (timing "新版本:\t\t" 
    (lambda () (repeat 100 (lambda () (iota-new 10000)))))
  (timing "c版本:\t\t"
    (lambda () (repeat 100 (lambda () (iota 10000)))))
  
  (display "\n")
  
  ;; 超大列表测试 (100000 个元素, 10 次)
  (display "超大列表 (100000 个元素, 10 次迭代):\n")
  (timing "旧版本:\t\t" 
    (lambda () (repeat 10 (lambda () (iota-old 100000)))))
  (timing "reverse版本:\t" 
    (lambda () (repeat 10 (lambda () (iota-reverse 100000)))))
  (timing "新版本:\t\t" 
    (lambda () (repeat 10 (lambda () (iota-new 100000)))))
  (timing "c版本:\t\t"
    (lambda () (repeat 10 (lambda () (iota 100000)))))
  
  (display "\n")
  
  ;; 非标准参数测试 (5000 个元素, start=10, step=3, 200 次)
  (display "非标准参数 (5000 个元素, start=10, step=3, 200 次迭代):\n")
  (timing "旧版本:\t\t" 
    (lambda () (repeat 200 (lambda () (iota-old 5000 10 3)))))
  (timing "reverse版本:\t" 
    (lambda () (repeat 200 (lambda () (iota-reverse 5000 10 3)))))
  (timing "新版本:\t\t" 
    (lambda () (repeat 200 (lambda () (iota-new 5000 10 3)))))
  (timing "c版本:\t\t"
    (lambda () (repeat 200 (lambda () (iota 5000 10 3)))))
)

;;; 运行测试
(verify)
(run-benchmarks)
