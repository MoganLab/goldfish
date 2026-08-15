(import (liii check)
        (goldfish compiler)
        (liii timeit))

;; L2-1 基准（构造性）：展示 pass 在常量密集型代码上的上限效果。
;;
;; 合成库：大量 (define x (+ a b)) / (define y (* c d)) 常量定义 +
;; 热点循环里 (if (> i 100) ...) 分支。折叠后常量定义直接变成字面量，
;; 循环里的常量比较在折叠时消除。

(define synthetic-defs
  '((define a (+ 1 2 3 4 5))
    (define b (* 2 3 4))
    (define c (- 100 42))
    (define d (string-length "hello world"))
    (define e (quotient 1000 7))
    (define f (modulo 1000 7))
    (define g (expt 2 10))
    (define h (min 100 200 50))
    (define i (max 1 2 3 4 5 6))
    (define j (abs -42))
    (define k (string-append "foo" "bar" "baz"))
    (define l (char->integer #\Z))
    (define m (integer->char 66))
    (define n (not #f))
    (define o (string->number "3.14"))
    (define p (symbol->string 'hello))
    (define q (string->symbol "world"))
    (define r (if (> 10 5) 100 200))
    (define s (if (< 1 0) 1 2))
    (define t (string-length (string-append "a" "b" "c")))
    (define u (+ 1 (* 2 (+ 3 4))))
    (define v (if (>= 6 6) (* 5 5) 0))
    (define w (if #f 1 2))
    (define x (if #t 9 8))
    (define y (+ 1000000 1))
    (define z (* 100 100))))

(let* ((compiled (compile-defs synthetic-defs (list constant-fold simplify-if)))
       (orig-size (call-with-output-string
                    (lambda (p) (for-each (lambda (d) (write d p)) synthetic-defs))))
       (comp-size (call-with-output-string
                    (lambda (p) (for-each (lambda (d) (write d p)) compiled)))))

  (newline)
  (display "=== 合成常量库（27 个 define）===\n")
  (display " 原 IR:     ") (display (string-length orig-size)) (display " 字符\n")
  (display " 编译 IR:   ") (display (string-length comp-size)) (display " 字符\n")
  (display " 缩减:      ")
  (display (- (string-length orig-size) (string-length comp-size)))
  (display " 字符 (")
  (display (* 100.0 (/ (- (string-length orig-size) (string-length comp-size))
                       (string-length orig-size))))
  (display "%)\n")

  (let loop ((ds synthetic-defs) (cs compiled) (n 0))
    (if (null? ds)
      (begin (display " 折叠:      ") (display n) (display "/")
             (display (length synthetic-defs)) (display " 个 define 被求值\n\n"))
      (begin
        (unless (equal? (car ds) (car cs))
          (set! n (+ n 1)))
        (loop (cdr ds) (cdr cs) n))))

  ;; 求值等价 + 速度：把折叠后的定义 eval，比较运行时的常量
  (let ((e-orig (inlet)) (e-comp (inlet)))
    (for-each (lambda (d) (eval d e-orig)) synthetic-defs)
    (for-each (lambda (d) (eval d e-comp)) compiled)
    ;; 27 个常量全部一致
    (define names (map cadr synthetic-defs))
    (for-each (lambda (n)
                (check (eval n e-orig) => (eval n e-comp)))
              names)

    ;; 速度：eval 常量引用（重复查 27 个绑定）
    (define (bench env)
      (lambda ()
        (for-each (lambda (n) (eval n env)) names)))
    (let ((t-orig (timeit (bench e-orig) '() 2000))
          (t-comp (timeit (bench e-comp) '() 2000)))
      (display "=== 常量引用求值耗时 (2000 次 x 27 绑定) ===\n")
      (display " 原 IR:     ") (display t-orig) (display " s\n")
      (display " 编译 IR:   ") (display t-comp) (display " s\n")
      (display " 加速:      ")
      (display (* 100.0 (/ (- t-orig t-comp) t-orig)))
      (display "%\n"))))

(check-report)
