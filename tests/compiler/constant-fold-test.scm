(import (liii check)
        (goldfish compiler)
        (goldfish core ir))

;; L2-1 单元测试：constant-fold 与 simplify-if pass 的行为（record IR 版）。

;; 辅助：sexp -> core->ir -> pass -> ir->core
(define (fold-sexp core pass) (ir->core (pass (core->ir core))))

;; 基本折叠
(check (fold-sexp '(define x (+ 1 2)) constant-fold) => '(define x 3))
(check (fold-sexp '(+ 1 (* 2 3)) constant-fold) => '7)
(check (fold-sexp '(string-append "a" "b") constant-fold) => '"ab")
(check (fold-sexp '(not #f) constant-fold) => '#t)
(check (fold-sexp '(char->integer #\A) constant-fold) => '65)
(check (fold-sexp '(integer->char 65) constant-fold) => '#\A)

;; 嵌套折叠深入 lambda 体内
(check (fold-sexp '(lambda (y) (+ y 2)) constant-fold) => '(lambda (y) (+ y 2)))
(check (fold-sexp '(lambda (y) (+ 2 3)) constant-fold) => '(lambda (y) 5))

;; quote 内容不被折叠（数据）
(check (fold-sexp '(quote (+ 1 2)) constant-fold) => '(quote (+ 1 2)))

;; 参数非常量则不折叠
(check (fold-sexp '(+ 1 x) constant-fold) => '(+ 1 x))
(check (fold-sexp '(+ x y) constant-fold) => '(+ x y))

;; 非折叠表中的函数不折叠
(check (fold-sexp '(car (quote (1 2))) constant-fold) => '(car (quote (1 2))))

;; 调用出错时不折叠（如除以零在折叠表中，但求值失败应保留原形式）
(check (fold-sexp '(quotient 1 0) constant-fold) => '(quotient 1 0))

;; define 的 RHS 折叠（curried define 规范化）
(check (fold-sexp '(define (f) (+ 1 2)) constant-fold) => '(define f (lambda () 3)))

;; simplify-if
(check (fold-sexp '(if #t 1 2) simplify-if) => '1)
(check (fold-sexp '(if #f 1 2) simplify-if) => '2)
(check (fold-sexp '(if x 1 2) simplify-if) => '(if x 1 2))
(check (fold-sexp '(if #t 1) simplify-if) => '1)
;; R7RS: (if #f 1) with no else arm returns an unspecified value, NOT #f,
;; so simplify-if must keep the if (it cannot fold to #f).
(check (fold-sexp '(if #f 1) simplify-if) => '(if #f 1))

;; 管线组合：折叠后化简
(check (ir->core (run-passes (core->ir '(define y (if (> 3 2) (+ 1 1) 0)))
                             (list constant-fold simplify-if)))
       => '(define y 2))
(check (ir->core (run-passes (core->ir '(define z (if (>= 7 3) (string-length "hello") -1)))
                             (list constant-fold simplify-if)))
       => '(define z 5))

;; compile-defs 对 defs 列表应用管线（sexp -> sexp 边界兼容）
(check (compile-defs '((define a (+ 1 1)) (define b (if #t 1 2)))
                     (list constant-fold simplify-if))
       => '((define a 2) (define b 1)))

;; 自求值结果直接输出，非自求值结果用 quote 包装
(check (fold-sexp '(quote foo) constant-fold) => '(quote foo))
(check (fold-sexp '42 constant-fold) => '42)

(check-report)
