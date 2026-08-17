(import (liii check)
        (goldfish compiler))

;; 自研字节码 VM 单元测试：VM 执行结果与 s7 eval 等价。

;; 加载一组 defs 到 VM（一个 program，函数注册为全局）
(define (vm-load-defs defs)
  (vm-load (to-bytecode (map core->ir defs)) #f))

(vm-load-defs '((define (add x y) (+ x y))
                (define (sub x y) (- x y))
                (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
                (define (loop i acc) (if (= i 0) acc (loop (- i 1) (+ acc 1))))
                (define (mk x) (lambda (y) (+ x y)))
                (define (f) (values 1 2))
                (define (g) (call-with-values (lambda () (values 1 2))
                              (lambda (a b) (+ a b))))
                (define (h x) (let ((a (+ x 1)) (b (* x 2))) (+ a b)))
                (define (id x) x)))

;; ===== 1. 基本算术调用 =====
(check (add 3 4) => 7)
(check (sub 10 3) => 7)
(check (add (add 1 2) (sub 5 1)) => 7)

;; ===== 2. 与 s7 eval 等价（对照）=====
(check (add 3 4) => ((lambda (x y) (+ x y)) 3 4))
(check (fact 5) => (letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))))
                     (f 5)))

;; ===== 3. 递归（VM 函数间调用）=====
(check (fact 5) => 120)
(check (fact 10) => 3628800)

;; ===== 4. 尾递归：VM 帧替换，深递归不爆栈 =====
(check (loop 100000 0) => 100000)
(check (loop 200000 7) => 200007)

;; ===== 5. 闭包捕获 =====
(check ((mk 5) 3) => 8)

;; ===== 6. 多值（VM 内 call-with-values 指令）=====
(check (g) => 3)
(check (call-with-values f (lambda (a b) (+ a b))) => 3)

;; ===== 7. let 槽与局部计算 =====
(check (h 3) => 10)   ; (3+1) + (3*2)
(check (h 10) => 31)

;; ===== 8. 恒等/常量 =====
(check (id 42) => 42)
(check (id "vm") => "vm")

;; ===== 9. 多 program：加载第二个 program 后，第一个的闭包仍有效 =====
(define f2 (vm-load (to-bytecode (list (core->ir '(lambda (x) (+ x 100))))) #f))
(check (f2 1) => 101)
(check (add 3 4) => 7)

(check-report)
