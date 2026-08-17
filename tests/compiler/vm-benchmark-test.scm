(import (liii check)
        (goldfish compiler)
        (liii timeit))

;; M1 基准：VM vs s7 eval 性能对比。

;; s7 eval 版
(define (fib-s7 n) (if (< n 2) n (+ (fib-s7 (- n 1)) (fib-s7 (- n 2)))))

;; VM 版
(vm-load (to-bytecode
          (map core->ir
               '((define (fib-vm n) (if (< n 2) n (+ (fib-vm (- n 1)) (fib-vm (- n 2)))))
                 (define (loop-vm i acc) (if (= i 0) acc (loop-vm (- i 1) (+ acc 1))))))))

;; 等价性先验证
(check (fib-s7 20) => 6765)
(check (fib-vm 20) => 6765)
(check (loop-vm 100000 0) => 100000)

;; ===== 1. 算术递归 fib(20) =====
(define (run-s7-fib) (fib-s7 20))
(define (run-vm-fib) (fib-vm 20))
(display "== fib(20) x500 ==") (newline)
(display "s7: ") (display (timeit run-s7-fib '() 500)) (newline)
(display "vm: ") (display (timeit run-vm-fib '() 500)) (newline)

;; ===== 2. 尾递归 loop 200000 =====
(define (run-s7-loop) (let loop ((i 200000) (acc 0))
                        (if (= i 0) acc (loop (- i 1) (+ acc 1)))))
(define (run-vm-loop) (loop-vm 200000 0))
(display "== tail loop 200000 x10 ==") (newline)
(display "s7: ") (display (timeit run-s7-loop '() 10)) (newline)
(display "vm: ") (display (timeit run-vm-loop '() 10)) (newline)

(check-report)
