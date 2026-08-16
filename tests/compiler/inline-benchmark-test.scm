(import (liii check)
        (goldfish compiler)
        (liii timeit))

(define synthetic-defs
  (list
    (list 'define 'f1
      '(lambda (x) ((lambda (g) (g (g x))) (lambda (y) (* y 2)))))
    (list 'define 'f2
      '(lambda (n)
         (let ((h (lambda (a b) (+ a b))))
           (letrec* ((loop (lambda (i acc)
                             (if (< i n) (loop (+ i 1) (h acc 1)) acc))))
             (loop 0 0)))))
    (list 'define 'f3
      '(lambda (lst)
         ((lambda (mapper)
            (mapper (lambda (v) (* v v)) lst))
          (lambda (f l)
            (if (null? l)
              '()
              (cons (f (car l)) (f3 (cdr l))))))))
    (list 'define 'g '(lambda (x) (f1 x)))))

(define compiled
  (compile-defs synthetic-defs (list constant-fold inline simplify-if)))

(define e-orig (inlet))
(define e-comp (inlet))
(for-each (lambda (d) (eval d e-orig)) synthetic-defs)
(for-each (lambda (d) (eval d e-comp)) compiled)

(define (size-of defs)
  (call-with-output-string (lambda (p) (for-each (lambda (d) (write d p)) defs))))
(newline)
(display "=== 函数内部内联效果 ===\n")
(display " 原 IR:   ") (display (string-length (size-of synthetic-defs))) (display " 字符\n")
(display " 编译 IR: ") (display (string-length (size-of compiled))) (display " 字符\n")

(define (call-in env name args)
  (let ((f (eval (list 'lambda '() (cons name args)) env)))
    (f)))
(check (call-in e-orig 'f1 '(5)) => (call-in e-comp 'f1 '(5)))
(check (call-in e-orig 'f2 '(100)) => (call-in e-comp 'f2 '(100)))
(check (call-in e-orig 'f3 '('(1 2 3))) => (call-in e-comp 'f3 '('(1 2 3))))
(check (call-in e-orig 'g '(5)) => (call-in e-comp 'g '(5)))

(define (bench-in env name args iters)
  (lambda ()
    (let loop ((i 0) (r #f))
      (if (< i iters) (loop (+ i 1) (call-in env name args)) r))))

(let ((t-orig (timeit (bench-in e-orig 'f1 '(5) 100000) '() 10))
      (t-comp (timeit (bench-in e-comp 'f1 '(5) 100000) '() 10)))
  (display "  f1 (100000x10): 原 ") (display t-orig) (display " / 编译 ")
  (display t-comp) (display " s  ")
  (display (* 100.0 (/ (- t-orig t-comp) t-orig))) (display "%\n"))

(let ((t-orig (timeit (bench-in e-orig 'f2 '(100) 5000) '() 10))
      (t-comp (timeit (bench-in e-comp 'f2 '(100) 5000) '() 10)))
  (display "  f2 (5000x10):   原 ") (display t-orig) (display " / 编译 ")
  (display t-comp) (display " s  ")
  (display (* 100.0 (/ (- t-orig t-comp) t-orig))) (display "%\n"))

(let ((t-orig (timeit (bench-in e-orig 'f3 '('(1 2 3 4 5)) 20000) '() 10))
      (t-comp (timeit (bench-in e-comp 'f3 '('(1 2 3 4 5)) 20000) '() 10)))
  (display "  f3 (20000x10):  原 ") (display t-orig) (display " / 编译 ")
  (display t-comp) (display " s  ")
  (display (* 100.0 (/ (- t-orig t-comp) t-orig))) (display "%\n"))

(check-report)
