(import (liii oop) (liii option2) (rename (liii rich-option) (rich-option option) (rich-none none)) (liii error) (liii check))

(check-true ((option 1) :defined?))
(check ((option 1) :get) => 1)
(check-false ((option '()) :defined?))
(check ((option '()) :get-or-else 1) => 1)

; ; 测试 option2 与 option 的等价性
(check-true ((option2 1) :defined?))
(check ((option2 1) :get) => 1)
(check-false ((option2 '()) :defined?))

; ; 测试 option 和 option2 的行为一致性
(define opt1 (option 42))
(define opt2 (option2 42))
(check (opt1 :get) => (opt2 :get))
(check (opt1 :defined?) => (opt2 :defined?))

(define opt3 (option '()))
(define opt4 (option2 '()))
(check (opt3 :defined?) => (opt4 :defined?))

(check ((option2 '()) :get-or-else 1) => 1)

;; 性能对比测试
(import (liii timeit))

(define (test-option-performance n)
  (let loop ((i 0) (sum 0))
    (if (>= i n)
        sum
        (let ((opt (option i)))
          (if (opt :defined?)
              (loop (+ i 1) (+ sum (opt :get)))
              (loop (+ i 1) sum))))))

(define (test-option2-performance n)
  (let loop ((i 0) (sum 0))
    (if (>= i n)
        sum
        (let ((opt (option2 i)))
          (if (opt :defined?)
              (loop (+ i 1) (+ sum (opt :get)))
              (loop (+ i 1) sum))))))

(define test-count 10000)

(display "=== Option vs Option2 性能对比测试 ===\n")
(display "测试次数: ")
(display test-count)
(newline)

(display "\n测试 option 性能:\n")
(let ((time (timeit (lambda () (test-option-performance test-count))
                    (lambda () #t)
                    1)))
  (display "执行时间: ")
  (display time)
  (display " 秒\n"))

(display "\n测试 option2 性能:\n")
(let ((time (timeit (lambda () (test-option2-performance test-count))
                    (lambda () #t)
                    1)))
  (display "执行时间: ")
  (display time)
  (display " 秒\n"))

;; 测试空值情况
(define (test-option-empty-performance n)
  (let loop ((i 0) (count 0))
    (if (>= i n)
        count
        (let ((opt (option '())))
          (if (not (opt :defined?))
              (loop (+ i 1) (+ count 1))
              (loop (+ i 1) count))))))

(define (test-option2-empty-performance n)
  (let loop ((i 0) (count 0))
    (if (>= i n)
        count
        (let ((opt (option2 '())))
          (if (not (opt :defined?))
              (loop (+ i 1) (+ count 1))
              (loop (+ i 1) count))))))

(display "\n=== 空值测试 ===\n")
(display "测试 option 空值性能:\n")
(let ((time (timeit (lambda () (test-option-empty-performance test-count))
                    (lambda () #t)
                    1)))
  (display "执行时间: ")
  (display time)
  (display " 秒\n"))

(display "\n测试 option2 空值性能:\n")
(let ((time (timeit (lambda () (test-option2-empty-performance test-count))
                    (lambda () #t)
                    1)))
  (display "执行时间: ")
  (display time)
  (display " 秒\n"))

;; Map 方法性能对比测试
(import (scheme time))

(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (display* msg (number->string (- end start)) "\n")))

(define (repeat n proc)
  (when (>= n 0)
        (proc)
        (repeat (- n 1) proc)))

(display "\n=== Map 方法性能对比测试 ===\n")
(display "测试次数: ")
(display test-count)
(newline)

(display "\n测试 option map 性能:\n")
(timing "执行时间: "
  (lambda () (repeat test-count (lambda () ((option 65536) :map (lambda (x) (+ x 1)))))))

(display "\n测试 option2 map 性能:\n")
(timing "执行时间: "
  (lambda () (repeat test-count (lambda () ((option2 65536) :map (lambda (x) (+ x 1)))))))

;; 测试空值的 map 方法性能
(display "\n=== 空值 Map 方法性能对比测试 ===\n")

(display "\n测试 option 空值 map 性能:\n")
(timing "执行时间: "
  (lambda () (repeat test-count (lambda () ((option '()) :map (lambda (x) (+ x 1)))))))

(display "\n测试 option2 空值 map 性能:\n")
(timing "执行时间: "
  (lambda () (repeat test-count (lambda () ((option2 '()) :map (lambda (x) (+ x 1)))))))
