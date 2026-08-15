(import (liii check)
        (goldfish compiler))

;; L2-1 单元测试：constant-fold 与 simplify-if pass 的行为。

;; 基本折叠
(check (constant-fold '(define x (+ 1 2))) => '(define x 3))
(check (constant-fold '(+ 1 (* 2 3))) => '7)
(check (constant-fold '(string-append "a" "b")) => '"ab")
(check (constant-fold '(not #f)) => '#t)
(check (constant-fold '(char->integer #\A)) => '65)
(check (constant-fold '(integer->char 65)) => '#\A)

;; 嵌套折叠深入 lambda 体内
(check (constant-fold '(lambda (y) (+ y 2))) => '(lambda (y) (+ y 2)))
(check (constant-fold '(lambda (y) (+ 2 3))) => '(lambda (y) 5))

;; quote 内容不被折叠（数据）
(check (constant-fold '(quote (+ 1 2))) => '(quote (+ 1 2)))

;; 参数非常量则不折叠
(check (constant-fold '(+ 1 x)) => '(+ 1 x))
(check (constant-fold '(+ x y)) => '(+ x y))

;; 非折叠表中的函数不折叠
(check (constant-fold '(car (quote (1 2)))) => '(car (quote (1 2))))

;; 调用出错时不折叠（如除以零在折叠表中，但求值失败应保留原形式）
(check (constant-fold '(quotient 1 0)) => '(quotient 1 0))

;; define 的 RHS 折叠
(check (constant-fold '(define (f) (+ 1 2))) => '(define (f) 3))

;; simplify-if
(check (simplify-if '(if #t 1 2)) => '1)
(check (simplify-if '(if #f 1 2)) => '2)
(check (simplify-if '(if x 1 2)) => '(if x 1 2))
(check (simplify-if '(if #t 1)) => '1)
(check (simplify-if '(if #f 1)) => '#f)

;; 管线组合：折叠后化简
(check (run-passes '(define y (if (> 3 2) (+ 1 1) 0))
                   (list constant-fold simplify-if))
       => '(define y 2))
(check (run-passes '(define z (if (>= 7 3) (string-length "hello") -1))
                   (list constant-fold simplify-if))
       => '(define z 5))

;; compile-defs 对 defs 列表应用管线
(check (compile-defs '((define a (+ 1 1)) (define b (if #t 1 2)))
                     (list constant-fold simplify-if))
       => '((define a 2) (define b 1)))

;; 自求值结果直接输出，非自求值结果用 quote 包装
(check (constant-fold '(string->symbol "foo")) => '(quote foo))
(check (constant-fold '(string->number "42")) => '42)

(check-report)
