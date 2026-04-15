(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; letrec
;; 创建递归绑定的局部变量，允许相互递归的函数定义。
;;
;; 语法
;; ----
;; (letrec ((var init) ...) body ...)
;;
;; 参数
;; ----
;; var : symbol
;; 变量名。
;;
;; init : any
;; 初始值表达式，通常是 lambda 表达式。
;;
;; body ... : any
;; 表达式体。
;;
;; 返回值
;; -----
;; any
;; 返回最后一个 body 表达式的结果。
;;
;; 说明
;; ----
;; letrec 允许在 init 表达式中引用其他 var，适合定义相互递归的函数。
;; 所有 var 在 init 求值前就已经绑定，但只有在使用时才求值。
;; 相互递归的偶数/奇数判断
(define (test-letrec)
  (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
           (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
          ) ;
    (list (even? 10) (odd? 10))
  ) ;letrec
) ;define
(check (test-letrec) => (list #t #f))
;; letrec 限制：init 表达式不能立即使用其他绑定值
(check-catch 'wrong-type-arg
  (letrec ((a 1) (b (+ a 1)))
    (list a b)
  ) ;letrec
) ;check-catch
(check-report)