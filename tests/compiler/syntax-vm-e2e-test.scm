(import (liii check)
        (goldfish)
        (goldfish compiler ir)
        (goldfish compiler bytecode)
        (goldfish expander syntax-ir))

;; 端到端闭环测试：expander 展开 → syntax->ir（含 primitive-ref/lexical-ref）
;; → to-bytecode → VM 执行。
;;
;; 与 vm-transformer-test（core->ir 手写 sexp）的区别：本测试从 expander
;; 的真实产物（syntax 对象）出发，覆盖 syntax->ir 的完整路径——binding
;; 访问、词法槽位分配、letrec 闭包捕获、Scheme map 回调等。

(define (vm-load-syntax exprs)
  (let*-values (((defs ctx) (expand-library-body
                             (map wrap-expression exprs)
                             the-base-library
                             (initial-context))))
    (let ((irs (map (lambda (d) (syntax->ir d ctx)) defs)))
      (vm-load (to-bytecode irs) #f)
      irs)))

;; ===== 1. 递归 + 条件 + 算术 =====
(define irs1 (vm-load-syntax
              '((define (a-fact n)
                  (if (= n 0) 1 (* n (a-fact (- n 1))))))))
(check (eval (list (define-name (car irs1)) 5) (rootlet)) => 120)
(check (eval (list (define-name (car irs1)) 0) (rootlet)) => 1)

;; ===== 2. Scheme map + VM closure 回调 =====
(define irs2 (vm-load-syntax
              '((define (a-double l)
                  (map (lambda (x) (* x 2)) l))
                (define (a-add2 a b)
                  (map + a b)))))
(check (eval (list (define-name (car irs2)) (list 'quote '(1 2 3))) (rootlet)) => '(2 4 6))
(check (eval (list (define-name (cadr irs2))
                   (list 'quote '(1 2)) (list 'quote '(10 20)))
             (rootlet))
       => '(11 22))

;; ===== 3. letrec 闭包 + 递归 + 列表构造（槽位分配）=====
(define irs3 (vm-load-syntax
              '((define (a-count n)
                  (letrec ((loop (lambda (i acc)
                                   (if (= i 0) acc (loop (- i 1) (cons i acc))))))
                    (loop n '()))))))
(check (eval (list (define-name (car irs3)) 4) (rootlet)) => '(1 2 3 4))

;; ===== 4. 嵌套 lambda 捕获 =====
(define irs4 (vm-load-syntax
              '((define (a-nest x)
                  (lambda (y) (+ x y))))))
(check ((eval (list (define-name (car irs4)) 10) (rootlet)) 5) => 15)
