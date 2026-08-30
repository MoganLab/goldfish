(import (liii check)
        (goldfish match)
        (goldfish compiler ir))

;; IR 转换与 record 匹配单元测试（权威 (goldfish core ir) API，无 shim 兼容层）。

;; 辅助：seq 树（二元右嵌套）展平为表达式列表。
(define (seq->list s)
  (let loop ((s s) (acc '()))
    (cond ((void? s) (reverse acc))
          ((seq? s) (loop (seq-tail s) (cons (seq-head s) acc)))
          (else (reverse (cons s acc))))))

;; 辅助：lambda 的 req 形参名（简单 case 无 opt/rest）。
(define (lambda-req ir)
  (lambda-case-req (lambda-body ir)))

;; 辅助：lambda body 表达式列表（seq 展平）。
(define (lambda-body-list ir)
  (seq->list (lambda-case-body (lambda-body ir))))

;; ===== 1. core->ir：原子保持，复合节点 record 化 =====
(check (let ((ir (core->ir '(+ x 1))))
         (list (call? ir)
               (call-proc ir)
               (call-args ir)))
       => '(#t + (x 1)))

(check (let ((ir (core->ir '(define (f x) (+ x 1)))))
         (list (toplevel-define? ir)
               (toplevel-define-name ir)
               (ir->core (toplevel-define-exp ir))))
       => '(#t f (lambda (x) (+ x 1))))

(check (let ((ir (core->ir '(lambda (x) (+ x 1)))))
         (list (lambda? ir)
               (lambda-req ir)
               (map ir->core (lambda-body-list ir))))
       => '(#t (x) ((+ x 1))))

(check (let ((ir (core->ir '(quote foo))))
         (list (const? ir) (const-exp ir)))
       => '(#t foo))
(check (core->ir '42) => 42)
(check (core->ir 'x) => 'x)

;; ===== 2. core->ir 嵌套结构 =====
(check (let ((ir (core->ir '(if (> x 0) 1 2))))
         (list (conditional? ir)
               (ir->core (conditional-test ir))
               (ir->core (conditional-consequent ir))
               (ir->core (conditional-alternate ir))))
       => '(#t (> x 0) 1 2))

;; if 无 else：else 为 #f
(check (let ((ir (core->ir '(if x (f)))))
         (list (conditional? ir) (conditional-alternate ir)))
       => '(#t #f))

;; let / letrec
(check (let ((ir (core->ir '(let ((a (+ x 1))) (* a 2)))))
         (list (let? ir)
               (map (lambda (n v) (list n (ir->core v)))
                    (let-names ir) (let-vals ir))
               (map ir->core (seq->list (let-body ir)))))
       => '(#t ((a (+ x 1))) ((* a 2))))

(check (let ((ir (core->ir '(letrec ((f (lambda () 1))) (f)))))
         (list (letrec? ir)
               (map (lambda (n v) (list n (ir->core v)))
                    (letrec-names ir) (letrec-vals ir))))
       => '(#t ((f (lambda () 1)))))

;; set! / values / call-with-values
(check (let ((ir (core->ir '(set! x 1))))
         (list (lexical-set? ir) (lexical-set-name ir) (ir->core (lexical-set-exp ir))))
       => '(#t x 1))

(check (let ((ir (core->ir '(values 1 2))))
         (list (values? ir) (map ir->core (values-args ir))))
       => '(#t (1 2)))

(check (let ((ir (core->ir '(call-with-values (lambda () (values 1 2))
                             (lambda (a b) (+ a b))))))
         (call-with-values? ir))
       => #t)

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
         ((? lambda? ir)
          (list 'lambda (lambda-req ir) (map ir->core (lambda-body-list ir))))
         (_ 'no))
       => '(lambda (x) ((+ x 1))))

(check (match (core->ir '(if (> x 0) 1 2))
         (($conditional test then else) (list 'if (ir->core test) then else))
         (_ 'no))
       => '(if (> x 0) 1 2))

(check (let ((ir (core->ir '(quote foo))))
         (match ir (($const v) (list 'const v)) (_ 'no)))
       => '(const foo))

(check (match (core->ir '(begin (a) (b)))
         ((? seq? s)
          (list 'begin (map ir->core (seq->list s))))
         (_ 'no))
       => '(begin ((a) (b))))

(check (match (core->ir '(values 1 2))
         (($values args) (list 'values (map ir->core args)))
         (_ 'no))
       => '(values (1 2)))

(check (match (core->ir '(call-with-values (lambda () (values 1 2))
                            (lambda (a b) (+ a b))))
         (($call-with-values producer consumer)
          (list 'call-with-values (ir->core producer) (ir->core consumer)))
         (_ 'no))
       => '(call-with-values (lambda () (values 1 2)) (lambda (a b) (+ a b))))

(check (match (core->ir '(set! x 1))
         (($lexical-set name depth index expr)
          (list 'set! name (ir->core expr)))
         (_ 'no))
       => '(set! x 1))

(check (match (core->ir '(define (f x) (+ x 1)))
         (($toplevel-define name value) (list 'define name (ir->core value)))
         (_ 'no))
       => '(define f (lambda (x) (+ x 1))))

;; ===== 5. core-language 契约 =====
;; core-form? 识别 core->ir 支持的全部 special-form 头（含派生形式）。
(check (map core-form? '(quote define lambda if begin let let* letrec
                          letrec* set! values call-with-values
                          module-ref module-set))
       => '(#t #t #t #t #t #t #t #t #t #t #t #t #t #t))

;; ===== 6. 深层匹配重写（模拟一个 pass 的骨架）=====
(check (match (core->ir '(f (g x)))
         (($call f (($call g (x)))) (list 'nested f g))
         (_ 'no))
       => '(nested f g))

(check-report)
