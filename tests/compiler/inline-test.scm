(import (liii check)
        (goldfish compiler))

;; L2-2 单元测试：inline pass（peval 核心：词法复制传播 + beta 归约）。

;; 常量传播到调用点
(check (inline '(let ((x 1)) (+ x 2))) => '(+ 1 2))

;; quote 数据传播
(check (inline '(let ((x '(a b))) (car x))) => '(car (quote (a b))))

;; 无副作用字面量可多次复制
(check (inline '(let ((x 5)) (list x x))) => '(list 5 5))

;; beta 归约：lambda 字面量应用
(check (inline '((lambda (x) (* x x)) 3)) => '(* 3 3))

;; beta 归约 + 参数传播 + 常量折叠协同（pipeline）
(check (run-passes '((lambda (x) (+ x 2)) 3) (list inline constant-fold))
       => '5)

;; 函数值经 lambda 归约的参数传入后再内联（高阶模式）
(check (run-passes '((lambda (f) (f 7)) (lambda (y) (* y y)))
                   (list inline constant-fold))
       => '49)

;; 非安全绑定（副作用调用）不传播：绑定保留
(check (inline '(let ((v (vector 1 2))) (vector-ref v 0)))
       => '(let ((v (vector 1 2))) (vector-ref v 0)))

;; 被 set! 的绑定不传播
(check (inline '(let ((x 1)) (set! x 2) (+ x 1)))
       => '(let ((x 1)) (set! x 2) (+ x 1)))

;; 自由变量不传播，调用保留
(check (inline '(f (quote a))) => '(f (quote a)))
(check (inline '(+ a b)) => '(+ a b))

;; quote 内容不被深入
(check (inline '(quote (let ((x 1)) x))) => '(quote (let ((x 1)) x)))

;; rest 参数归约：参数列表构造保留为绑定
(check (inline '((lambda (a . rest) (cons a rest)) 1 2 3))
       => '(let ((rest (list 2 3))) (cons 1 rest)))

;; 参数个数不匹配：不归约，原样保留
(check (inline '((lambda (x) x) 1 2)) => '((lambda (x) x) 1 2))

;; 单参数 lambda (lambda x body)
(check (inline '((lambda x x) 9)) => '9)

;; lambda 值经 let 绑定传播（闭包复制）
(check (run-passes '(let ((f (lambda (n) (+ n 1)))) (f 10))
                   (list inline constant-fold))
       => '11)

;; 递归函数不传播：保留调用到 letrec 绑定，避免无界展开
(check (inline '((lambda (x)
                   (letrec* ((f (lambda (y) (if (= y 1) y (f (- y 1))))))
                     (f x)))
                  5))
       => '(letrec* ((f (lambda (y) (if (= y 1) y (f (- y 1)))))) (f 5)))

;; 嵌套 letrec：外层参数传播进递归闭包，递归内层保留
(check (inline '((lambda (a)
                   (letrec* ((g (lambda (b) (if (= b 0) a (g (- b 1))))))
                     (g a)))
                  7))
       => '(letrec* ((g (lambda (b) (if (= b 0) 7 (g (- b 1)))))) (g 7)))

(check-report)
