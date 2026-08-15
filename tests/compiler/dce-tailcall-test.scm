(import (liii check)
        (goldfish compiler))

;; L2-3 pass 扩展：尾调用标记 + 死代码消除 单元测试。

;; ===== 1. tail-call-positions =====
;; if 分支是尾位置
(check (tail-call-positions '(lambda (x) (if (> x 0) (f x) (g x))))
       => '(lambda (x) (if (> x 0) (tail-call (f x)) (tail-call (g x)))))

;; begin 尾位置
(check (tail-call-positions '(lambda (x) (begin (set! y 1) (h x))))
       => '(lambda (x) (begin (set! y 1) (tail-call (h x)))))

;; let 尾位置（let body 是列表，标记在 body 上）
(check (tail-call-positions '(lambda (x) (let ((a 1)) (+ a x))))
       => '(lambda (x) (let ((a 1)) ((tail-call (+ a x))))))

;; call-with-values 的 consumer 调用是尾位置
(check (tail-call-positions '(lambda (x) (call-with-values (lambda () (p x))
                                        (lambda (a b) (k a b)))))
       => '(lambda (x) (call-with-values (lambda () (p x))
                       (lambda (a b) (tail-call (k a b))))))

;; 非尾位置不标记：if 的 test、lambda 的非末表达式
(check (tail-call-positions '(lambda (x) (if (t x) (f x) (g x))))
       => '(lambda (x) (if (t x) (tail-call (f x)) (tail-call (g x)))))

;; quote 内容不动
(check (tail-call-positions '(lambda (x) '(+ 1 2)))
       => '(lambda (x) (quote (+ 1 2))))

;; ===== 2. eliminate-dead-defs =====
;; 未被引用的 lambda def 删除；used 被 main 引用保留
(check (eliminate-dead-defs '((define used (lambda (x) (+ x 1)))
                               (define unused (lambda (y) (* y 2)))
                               (define main (lambda () (used 1)))
                               (register main)))
       => '((define used (lambda (x) (+ x 1)))
            (define main (lambda () (used 1)))
            (register main)))

;; 引用链：register 引用 b，b 引用 a，都保留
(check (eliminate-dead-defs '((define a (lambda () 1))
                               (define b (lambda () (a)))
                               (register b)))
       => '((define a (lambda () 1))
            (define b (lambda () (a)))
            (register b)))

;; 自我递归的 def 若被引用保留
(check (eliminate-dead-defs '((define loop (lambda (i) (if (= i 0) 0 (loop (- i 1)))))
                               (register loop)))
       => '((define loop (lambda (i) (if (= i 0) 0 (loop (- i 1)))))
            (register loop)))

;; 非 lambda 值定义：即使未引用也保留（可能有副作用）
(check (eliminate-dead-defs '((define v (list 1 2))
                               (define w (lambda () 1))
                               (register v)))
       => '((define v (list 1 2)) (register v)))

;; fixpoint：a 未引用但 b 引用 a，c 引用 b，register 引用 c —— 全保留
(check (eliminate-dead-defs '((define a (lambda () 1))
                               (define b (lambda () (a)))
                               (define c (lambda () (b)))
                               (register c)))
       => '((define a (lambda () 1))
            (define b (lambda () (a)))
            (define c (lambda () (b)))
            (register c)))

(check-report)
