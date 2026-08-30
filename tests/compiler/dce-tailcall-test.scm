(import (liii check)
        (goldfish compiler)
        (goldfish compiler ir))

;; L2-3 pass 扩展：尾调用标记 + 死代码消除 单元测试（record IR 版）。

;; 辅助：tail-call-positions 输出转回带标记 sexp（(tail-call ...) 包装）
;; 自实现递归，识别 (tail-call <ir>) 标记（ir->core 不处理它）。
(define (tc-sexp ir)
  (cond
    ((pair? ir)
     (if (eq? (car ir) 'tail-call)
       (list 'tail-call (tc-sexp (cadr ir)))
       (map tc-sexp ir)))
    ((const? ir)
     (let ((v (const-value ir)))
       (if (or (number? v) (string? v) (char? v) (boolean? v)
               (null? v) (eof-object? v))
         v
         (list 'quote v))))
    ((lambda? ir)
     (let ((bs (lambda-body ir)))
       (cons 'lambda
             (cons (lambda-formals ir)
                   (if (and (pair? bs) (null? (cdr bs)))
                     (map tc-sexp bs)
                     (list (cons 'begin (map tc-sexp bs))))))))
    ((if? ir)
     (list 'if (tc-sexp (if-test ir)) (tc-sexp (if-then ir))
           (if (if-else ir) (tc-sexp (if-else ir)) #f)))
    ((begin? ir)
     (cons 'begin (map tc-sexp (begin-body ir))))
    ((let? ir)
     (list 'let
           (map (lambda (b) (list (car b) (tc-sexp (cadr b))))
                (let-bindings ir))
           (map tc-sexp (let-body ir))))
    ((letrec? ir)
     (list 'letrec
           (map (lambda (b) (list (car b) (tc-sexp (cadr b))))
                (letrec-bindings ir))
           (map tc-sexp (letrec-body ir))))
    ((set!? ir)
     (list 'set! (set!-target ir) (tc-sexp (set!-expr ir))))
    ((call-with-values? ir)
     (list 'call-with-values (tc-sexp (cwv-producer ir))
           (tc-sexp (cwv-consumer ir))))
    ((values? ir)
     (cons 'values (map tc-sexp (values-args ir))))
    ((call? ir)
     (cons (tc-sexp (call-proc ir)) (map tc-sexp (call-args ir))))
    (else (ir->core ir))))

;; 辅助：DCE 的 sexp 接口（defs 列表）
(define (dce defs)
  (map ir->core (eliminate-dead-defs (map core->ir defs))))

;; ===== 1. tail-call-positions =====
;; if 分支是尾位置
(check (tc-sexp (tail-call-positions (core->ir '(lambda (x) (if (> x 0) (f x) (g x))))))
       => '(lambda (x) (if (> x 0) (tail-call (f x)) (tail-call (g x)))))

;; begin 尾位置
(check (tc-sexp (tail-call-positions (core->ir '(lambda (x) (begin (set! y 1) (h x))))))
       => '(lambda (x) (begin (set! y 1) (tail-call (h x)))))

;; let 尾位置（let body 是列表，标记在 body 上）
(check (tc-sexp (tail-call-positions (core->ir '(lambda (x) (let ((a 1)) (+ a x))))))
       => '(lambda (x) (let ((a 1)) ((tail-call (+ a x))))))

;; call-with-values 是普通调用（派生形式，见 ir.scm），整调用在尾位置
(check (tc-sexp (tail-call-positions
                 (core->ir '(lambda (x) (call-with-values (lambda () (p x))
                                        (lambda (a b) (k a b)))))))
       => '(lambda (x) (tail-call (call-with-values (lambda () (p x))
                                   (lambda (a b) (k a b))))))

;; 非尾位置不标记：if 的 test、lambda 的非末表达式
(check (tc-sexp (tail-call-positions (core->ir '(lambda (x) (if (t x) (f x) (g x))))))
       => '(lambda (x) (if (t x) (tail-call (f x)) (tail-call (g x)))))

;; quote 内容不动
(check (tc-sexp (tail-call-positions (core->ir '(lambda (x) '(+ 1 2)))))
       => '(lambda (x) (quote (+ 1 2))))

;; ===== 2. eliminate-dead-defs =====
;; 未被引用的 lambda def 删除；used 被 main 引用保留
(check (dce '((define used (lambda (x) (+ x 1)))
              (define unused (lambda (y) (* y 2)))
              (define main (lambda () (used 1)))
              (register main)))
       => '((define used (lambda (x) (+ x 1)))
            (define main (lambda () (used 1)))
            (register main)))

;; 引用链：register 引用 b，b 引用 a，都保留
(check (dce '((define a (lambda () 1))
              (define b (lambda () (a)))
              (register b)))
       => '((define a (lambda () 1))
            (define b (lambda () (a)))
            (register b)))

;; 自我递归的 def 若被引用保留
(check (dce '((define loop (lambda (i) (if (= i 0) 0 (loop (- i 1)))))
              (register loop)))
       => '((define loop (lambda (i) (if (= i 0) 0 (loop (- i 1)))))
            (register loop)))

;; 非 lambda 值定义：即使未引用也保留（可能有副作用）
(check (dce '((define v (list 1 2))
              (define w (lambda () 1))
              (register v)))
       => '((define v (list 1 2)) (register v)))

;; fixpoint：a 未引用但 b 引用 a，c 引用 b，register 引用 c —— 全保留
(check (dce '((define a (lambda () 1))
              (define b (lambda () (a)))
              (define c (lambda () (b)))
              (register c)))
       => '((define a (lambda () 1))
            (define b (lambda () (a)))
            (define c (lambda () (b)))
            (register c)))

(check-report)
