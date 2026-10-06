(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; 1. 基础语法模板与常数
(check (syntax (+ 1 2)) => '(+ 1 2))
(check (syntax "hello") => "hello")
(check (syntax 42) => 42)

;; 2. 模式变量绑定
(check (syntax-case 'foo ()
         (x (syntax x)))
       => 'foo)

;; 3. 点对与列表解构
(check (syntax-case '(a . b) ()
         ((x . y) (syntax (x y))))
       => '(a b))

;; 4. 字面量匹配
(check (syntax-case '(a . b) (b)
         ((b . y) #f)
         ((x . b) (syntax x)))
       => 'a)

;; 5. 简单省略号
(check (syntax-case '(a b c) ()
         ((a ...) (syntax (a ...))))
       => '(a b c))

;; 6. 带后置元素的省略号
(check (syntax-case '(a b c) ()
         ((a ... b) (syntax (a ... x b))))
       => '(a b x c))

;; 7. 带点尾后置元素的省略号
(check (syntax-case '(a b c . d) ()
         ((a ... b . c) (syntax (a ... x b y c))))
       => '(a b x c y d))

;; 8. 嵌套省略号解构与重排
(check (syntax-case '((a b c) (d e f)) ()
         (((x ... y) ...) (syntax ((x ...) ... y ...))))
       => '((a b) (d e) c f))

;; 9. quasisyntax / unsyntax / unsyntax-splicing
(check (quasisyntax (list (unsyntax (+ 1 2))))
       => '(list 3))

(check (quasisyntax (list (unsyntax-splicing (list 1 2 3))))
       => '(list 1 2 3))

;; 10. with-syntax 局部绑定
(check (with-syntax (((a b) '(10 20)))
         (quasisyntax (sum (unsyntax (+ (syntax a) (syntax b))))))
       => '(sum 30))

;; 11. datum->syntax 与 syntax->datum
(let ((stx (datum->syntax 'here '(foo 1 2))))
  (check (syntax->datum stx) => '(foo 1 2)))

;; 12. 标识符谓词与 generate-temporaries
(check-true (free-identifier=? 'a 'a))
(check-false (free-identifier=? 'a 'b))
(check-true (bound-identifier=? 'a 'a))
(check-false (bound-identifier=? 'a 'b))
(check-true (identifier? 'a))
(check-false (identifier? 123))

(let ((temps (generate-temporaries '(a b c))))
  (check (length temps) => 3)
  (check-true (identifier? (car temps)))
  (check-false (eq? (car temps) (cadr temps))))

;; 13. 验收标准测试项 1：基于 define-syntax 的过程式计算宏
(define-syntax calc-add
  (lambda (stx)
    (syntax-case stx ()
      ((_ a b)
       (let ((sum (+ (syntax->datum (syntax a))
                     (syntax->datum (syntax b)))))
         (quasisyntax (list (unsyntax sum))))))))

(check (calc-add 10 20) => '(30))

;; 14. 模式不匹配时抛出 syntax-error
(check-catch 'syntax-error
  (syntax-case '(1 2 3) ()
    ((a b) 'matched)))

;; 15. 带守卫表达式（fender）的分支分流
(check (syntax-case '(foo 5) ()
         ((_ n) (and (number? (syntax->datum (syntax n)))
                     (even? (syntax->datum (syntax n))))
          'even)
         ((_ n) (and (number? (syntax->datum (syntax n)))
                     (odd? (syntax->datum (syntax n))))
          'odd))
       => 'odd)

;; 16. 空省略号匹配
(check (syntax-case '() ()
         ((a ...) (syntax (a ...))))
       => '())

;; 17. 向量模板与解构
(check (syntax-case '#(1 2 3) ()
         (#(a b c) (syntax #(c b a))))
       => '#(3 2 1))

;; 18. syntax-violation
(check-catch 'syntax-error
  (syntax-violation 'my-macro "something wrong" '(bad form)))

;; 19. 导出宏引用库内未导出的私有过程（验证卫生隔离）
(define-library (test private-syntax-case-helper)
  (export exported-sc-macro)
  (import (scheme base) (liii syntax-case))
  (begin
    (define (private-sc-add10 x) (+ x 10))
    (define-syntax exported-sc-macro
      (lambda (stx)
        (syntax-case stx ()
          ((_ x) (syntax (private-sc-add10 x))))))))

(import (test private-syntax-case-helper))
(check (exported-sc-macro 5) => 15)
(check-catch 'unbound-variable (private-sc-add10 5))

(check-report)
