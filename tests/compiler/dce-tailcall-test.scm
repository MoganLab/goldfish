(import (liii check)
        (goldfish compiler)
        (goldfish core ir))

;; L2-3 pass 扩展：尾调用标记 + 死代码消除 单元测试（record IR 版）。

;; 辅助：tail-call-positions 输出转回带标记 sexp（(tail-call ...) 包装）
;; 自实现递归，识别 (tail-call <ir>) 标记（ir->core 不处理它）。
;; 全部使用权威 (goldfish core ir) API：<lambda> body 是 <lambda-case>，
;; <begin> 是二元右嵌套 <seq>，<set!> 是 <lexical-set>。

;; seq 树展平为表达式列表。
(define (seq->list s)
  (let loop ((s s) (acc '()))
    (cond ((void? s) (reverse acc))
          ((seq? s) (loop (seq-tail s) (cons (seq-head s) acc)))
          (else (reverse (cons s acc))))))

;; arity 分量重建 formals 列表。
(define (arity->formals req opt rest)
  (cond ((and (null? opt) rest) (append req rest))
        ((and (null? opt) (not rest)) req)
        (else (append req opt (if rest (list rest) '())))))

(define (tc-sexp ir)
  (cond
    ((pair? ir)
     (if (eq? (car ir) 'tail-call)
       (list 'tail-call (tc-sexp (cadr ir)))
       (map tc-sexp ir)))
    ((const? ir)
     (let ((v (const-exp ir)))
       (if (or (number? v) (string? v) (char? v) (boolean? v)
               (null? v) (eof-object? v))
         v
         (list 'quote v))))
    ((lambda? ir)
     (let* ((lc (lambda-body ir))
            (bs (seq->list (lambda-case-body lc))))
       (cons 'lambda
             (cons (arity->formals (lambda-case-req lc)
                                   (lambda-case-opt lc)
                                   (lambda-case-rest lc))
                   (if (and (pair? bs) (null? (cdr bs)))
                     (map tc-sexp bs)
                     (list (cons 'begin (map tc-sexp bs))))))))
    ((conditional? ir)
     (list 'if (tc-sexp (conditional-test ir))
           (tc-sexp (conditional-consequent ir))
           (if (conditional-alternate ir) (tc-sexp (conditional-alternate ir)) #f)))
    ((seq? ir)
     (cons 'begin (map tc-sexp (seq->list ir))))
    ((let? ir)
     (list 'let
           (map (lambda (n v) (list n (tc-sexp v)))
                (let-names ir) (let-vals ir))
           (map tc-sexp (seq->list (let-body ir)))))
    ((letrec? ir)
     (list 'letrec
           (map (lambda (n v) (list n (tc-sexp v)))
                (letrec-names ir) (letrec-vals ir))
           (map tc-sexp (seq->list (letrec-body ir)))))
    ((lexical-set? ir)
     (list 'set! (lexical-set-name ir) (tc-sexp (lexical-set-exp ir))))
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
