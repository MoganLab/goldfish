(import (liii check)
        (goldfish compiler)
        (goldfish)
        (goldfish expander tree-il))

;; 自研字节码 VM 单元测试：VM 执行结果与 s7 eval 等价。

;; 加载一组 defs 到 VM（一个 program，函数注册为全局），返回 IR 列表。
;; defs 经 expander 直出 IR（core->ir 已退役）；syntax->ir 的顶层 define
;; 名是 gensym（add:0），所以用 toplevel-define-name 取 VM 注册的全局名。
(define (vm-load-defs defs)
  (let*-values (((ds ctx) (expand-library-body
                           (map wrap-expression defs)
                           the-base-library
                           (initial-context))))
    (let ((irs (map (lambda (d) (syntax->ir d ctx)) ds)))
      (vm-load (encode-bytecode (to-bytecode irs)) #f)
      irs)))

;; 单个 datum（define 或表达式）-> IR
(define (sexp->ir core)
  (let*-values (((defs ctx) (expand-library-body
                             (list (wrap-expression core))
                             the-base-library
                             (initial-context))))
    (syntax->ir (car defs) ctx)))

;; VM 注册的全局函数（用 gensym 名从 rootlet 取）
(define (vm-global ir) (eval (toplevel-define-name ir) (rootlet)))

(define irs (vm-load-defs '((define (add x y) (+ x y))
                            (define (sub x y) (- x y))
                            (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
                            (define (loop i acc) (if (= i 0) acc (loop (- i 1) (+ acc 1))))
                            (define (mk x) (lambda (y) (+ x y)))
                            (define (f) (values 1 2))
                            (define (g) (call-with-values (lambda () (values 1 2))
                                          (lambda (a b) (+ a b))))
                            (define (h x) (let ((a (+ x 1)) (b (* x 2))) (+ a b)))
                            (define (id x) x))))
(define add (vm-global (list-ref irs 0)))
(define sub (vm-global (list-ref irs 1)))
(define fact (vm-global (list-ref irs 2)))
(define loop (vm-global (list-ref irs 3)))
(define mk (vm-global (list-ref irs 4)))
(define f (vm-global (list-ref irs 5)))
(define g (vm-global (list-ref irs 6)))
(define h (vm-global (list-ref irs 7)))
(define id (vm-global (list-ref irs 8)))

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
(define f2ir (sexp->ir '(lambda (x) (+ x 100))))
(define f2 (vm-load (encode-bytecode (to-bytecode (list f2ir))) #f))
(check (f2 1) => 101)
(check (add 3 4) => 7)

;; ===== 10. rest 参数 =====
(define rest-f-ir (sexp->ir '(define (rest-f . args) (length args))))
(vm-load (encode-bytecode (to-bytecode (list rest-f-ir))) #f)
(define rest-f (vm-global rest-f-ir))
(check (rest-f 1 2 3) => 3)
(check (rest-f) => 0)

(check-report)
