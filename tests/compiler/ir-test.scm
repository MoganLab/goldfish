(import (liii check)
        (goldfish match)
        (goldfish compiler ir))

;; IR 转换与 record 匹配单元测试。

;; ===== 1. core->ir：原子保持，复合节点 record 化 =====
(check (let ((ir (core->ir '(+ x 1))))
         (list (call? ir)
               (call-proc ir)
               (call-args ir)))
       => '(#t + (x 1)))

(check (let ((ir (core->ir '(define (f x) (+ x 1)))))
         (list (define? ir)
               (define-name ir)
               (ir->core (define-value ir))))
       => '(#t f (lambda (x) (+ x 1))))

(check (let ((ir (core->ir '(lambda (x) (+ x 1)))))
         (list (lambda? ir)
               (lambda-formals ir)
               (map ir->core (lambda-body ir))))
       => '(#t (x) ((+ x 1))))

(check (let ((ir (core->ir '(quote foo))))
         (list (const? ir) (const-value ir)))
       => '(#t foo))
(check (core->ir '42) => 42)
(check (core->ir 'x) => 'x)

;; ===== 2. core->ir 嵌套结构 =====
(check (let ((ir (core->ir '(if (> x 0) 1 2))))
         (list (if? ir)
               (ir->core (if-test ir))
               (ir->core (if-then ir))
               (ir->core (if-else ir))))
       => '(#t (> x 0) 1 2))

;; if 无 else：else 为 #f
(check (let ((ir (core->ir '(if x (f)))))
         (list (if? ir) (if-else ir)))
       => '(#t #f))

;; let / letrec
(check (let ((ir (core->ir '(let ((a (+ x 1))) (* a 2)))))
         (list (let? ir)
               (map (lambda (b) (list (car b) (ir->core (cadr b))))
                    (let-bindings ir))
               (map ir->core (let-body ir))))
       => '(#t ((a (+ x 1))) ((* a 2))))

(check (let ((ir (core->ir '(letrec ((f (lambda () 1))) (f)))))
         (list (letrec? ir)
               (map (lambda (b) (list (car b) (ir->core (cadr b))))
                    (letrec-bindings ir))))
       => '(#t ((f (lambda () 1)))))

;; set! / values / call-with-values
(check (let ((ir (core->ir '(set! x 1))))
         (list (set!? ir) (set!-target ir) (set!-expr ir)))
       => '(#t x 1))

(check (let ((ir (core->ir '(values 1 2))))
         (list (call? ir) (call-proc ir) (call-args ir)))
       => '(#t values (1 2)))

(check (let ((ir (core->ir '(call-with-values (lambda () (values 1 2))
                             (lambda (a b) (+ a b))))))
         (list (call? ir) (call-proc ir)))
       => '(#t call-with-values))

;; ===== 3. ir->core 往返 =====
(check (let ((core '(if (> x 0) (f (- x 1)) 0)))
         (equal? (ir->core (core->ir core)) core))
       => #t)

(check (let ((core '((define f (lambda (x) (let ((a (+ x 1))) (* a 2)))))))
         (equal? (map (lambda (d) (ir->core (core->ir d))) core) core))
       => #t)

;; ===== 4. record 匹配（$ 模式，跨库 pattern-syntax）=====
(check (match (core->ir '(+ x 1))
         (($call proc args) (list 'call proc args))
         (_ 'no))
       => '(call + (x 1)))

(check (match (core->ir '(lambda (x) (+ x 1)))
         (($lambda formals body) (list 'lambda formals (map ir->core body)))
         (_ 'no))
       => '(lambda (x) ((+ x 1))))

(check (match (core->ir '(if (> x 0) 1 2))
         (($if test then else) (list 'if (ir->core test) then else))
         (_ 'no))
       => '(if (> x 0) 1 2))

(check (let ((ir (core->ir '(quote foo))))
         (match ir (($const v) (list 'const v)) (_ 'no)))
       => '(const foo))

(check (match (core->ir '(begin (a) (b)))
         (($begin body) (list 'begin (map ir->core body)))
         (_ 'no))
       => '(begin ((a) (b))))

(check (match (core->ir '(values 1 2))
         (($call proc args) (list 'call proc args))
         (_ 'no))
       => '(call values (1 2)))

(check (match (core->ir '(set! x 1))
         (($set! name expr) (list 'set! name expr))
         (_ 'no))
       => '(set! x 1))

(check (match (core->ir '(define (f x) (+ x 1)))
         (($define name value) (list 'define name (ir->core value)))
         (_ 'no))
       => '(define f (lambda (x) (+ x 1))))

;; ===== 5. 深层匹配重写（模拟一个 pass 的骨架）=====
(check (match (core->ir '(f (g x)))
         (($call f (($call g (x)))) (list 'nested f g))
         (_ 'no))
       => '(nested f g))

(check-report)
