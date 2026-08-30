(import (liii check)
        (goldfish compiler)
        (goldfish core ir)
        (goldfish)
        (goldfish expander tree-il))

;; L2-1 单元测试：constant-fold 与 simplify-if pass 的行为（record IR 版）。

;; 辅助：datum -> expander 直出 IR -> pass -> ir->core
;; （core->ir 已退役；syntax->ir 的名字是 gensym（x:0），normalize-names
;; 去掉 :数字 后缀，断言用源名，避免依赖展开顺序）
(define (sexp->ir core)
  (let*-values (((defs ctx) (expand-library-body
                             (list (wrap-expression core))
                             the-base-library
                             (initial-context))))
    (syntax->ir (car defs) ctx)))
(define (normalize-names x)
  (cond
    ((symbol? x)
     (let* ((s (symbol->string x))
            (n (string-length s)))
       (let loop ((i (- n 1)))
         (if (and (>= i 0) (char-numeric? (string-ref s i)))
           (loop (- i 1))
           (if (and (>= i 0) (char=? (string-ref s i) #\:)
                    (< i (- n 1)))
             (string->symbol (substring s 0 i))
             x)))))
    ((pair? x) (cons (normalize-names (car x)) (normalize-names (cdr x))))
    ((vector? x) (vector-map normalize-names x))
    (else x)))
(define (fold-sexp core pass) (normalize-names (ir->core (pass (sexp->ir core)))))

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
(check (fold-sexp '(if #f 1) simplify-if) => '(if #f #f))

;; 管线组合：折叠后化简
(check (normalize-names (ir->core (run-passes (sexp->ir '(define y (if (> 3 2) (+ 1 1) 0)))
                                              (list constant-fold simplify-if))))
       => '(define y 2))
(check (normalize-names (ir->core (run-passes (sexp->ir '(define z (if (>= 7 3) (string-length "hello") -1)))
                                              (list constant-fold simplify-if))))
       => '(define z 5))

(check (fold-sexp '(quote foo) constant-fold) => '(quote foo))
(check (fold-sexp '42 constant-fold) => '42)

(check-report)
