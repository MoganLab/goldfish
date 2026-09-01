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
(define (pipe core ps) (normalize-names (ir->core (run-passes (sexp->ir core) ps))))

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

;; ===== inline (peval core): copy propagation + beta reduction + folding =====

;; Propagation exposes a primitive call whose args are now constants; inline
;; folds it (constant-fold alone cannot, it runs before propagation).
(check (pipe '(define x (let ((a 13)) (* a a)))
             (list constant-fold inline simplify-if))
       => '(define x 169))
(check (pipe '(define x (* 13 13))
             (list constant-fold inline simplify-if))
       => '(define x 169))
;; The wingolog peval article's worked example.
(check (pipe '(define r (let ((x 13)) (* x foo)))
             (list constant-fold inline simplify-if))
       => '(define r (* 13 foo)))
;; beta reduction folds through.
(check (pipe '(define r ((lambda (x) (+ x 2)) 3))
             (list constant-fold inline simplify-if))
       => '(define r 5))
;; nested non-recursive inlining (compose).
(check (pipe '(define r (let ((inc (lambda (n) (+ n 1))))
                         (inc (inc (inc 0)))))
             (list constant-fold inline simplify-if))
       => '(define r 3))
;; conditional folding inside inline (dead branch not walked).
(check (pipe '(define r (if (= 3 0) 1 2))
             (list constant-fold inline simplify-if))
       => '(define r 2))
(check (pipe '(define r (let ((c 3)) (if (= c 0) 1 c)))
             (list constant-fold inline simplify-if))
       => '(define r 3))

;; ===== recursion unrolling (budget-bounded peval) =====

;; A recursive letrec with a constant argument folds to a constant and the
;; dead binding disappears.
(check (pipe '(define r (letrec ((loop (lambda (n) (if (= n 0) 1 (* n (loop (- n 1)))))))
                                   (loop 3)))
             (list constant-fold inline simplify-if))
       => '(define r 6))
;; Named let (the s7 lowering ((letrec ((lp lam)) lp) 3 1)) folds the same.
(check (pipe '(define r (let lp ((n 3) (acc 1))
                          (if (= n 0) acc (lp (- n 1) (* acc n)))))
             (list constant-fold inline simplify-if))
       => '(define r 6))
;; Mutual recursion folds through a known argument.
(check (pipe '(define r (letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
                                 (odd  (lambda (n) (if (= n 0) #f (even (- n 1))))))
                            (even 10)))
             (list constant-fold inline simplify-if))
       => '(define r #t))
;; An unknown argument is NOT unrolled: the recursive call stays a variable
;; call (no residual bloat).
(check (pipe '(define (f n)
                (letrec ((loop (lambda (n) (if (= n 0) 1 (* n (loop (- n 1)))))))
                  (loop n)))
             (list constant-fold inline simplify-if))
       => '(define f (lambda (n) (letrec ((loop (lambda (n) (if (= n 0) 1 (* n (loop (- n 1))))))) (loop n)))))

(check-report)
