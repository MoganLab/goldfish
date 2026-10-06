(import (scheme base)
        (srfi 78))

(check-set-mode! 'report-failed)

;; define-syntax
;; 在当前词法作用域中定义顶层语法关键字并绑定宏转换器。
;;
;; 语法
;; ----
;; (define-syntax <keyword> <transformer-spec>)
;;
;; 参数
;; ----
;; <keyword> : identifier
;; 要绑定的语法关键字标识符。
;;
;; <transformer-spec> : transformer
;; 宏转换器规格说明，通常为 (syntax-rules ...) 表达式。
;;
;; 返回值
;; -----
;; 未指定值。

;; 测试1：基础常量宏
(define-syntax test-const
  (syntax-rules ()
    ((test-const) 100)))

(check (test-const) => 100)

;; 测试2：带单参数的语法宏
(define-syntax test-inc
  (syntax-rules ()
    ((test-inc x) (+ x 1))))

(check (test-inc 5) => 6)
(check (test-inc (* 2 3)) => 7)

;; 测试3：条件展开宏 when-test
(define-syntax when-test
  (syntax-rules ()
    ((when-test test e1 e2 ...)
     (if test (begin e1 e2 ...)))))

(check (when-test (= 1 1) (+ 10 20)) => 30)
(check (when-test (= 1 1) 42) => 42)

;; 测试4：多分支模式宏
(define-syntax my-cond-branch
  (syntax-rules ()
    ((my-cond-branch 1) 'one)
    ((my-cond-branch 2) 'two)
    ((my-cond-branch any) 'other)))

(check (my-cond-branch 1) => 'one)
(check (my-cond-branch 2) => 'two)
(check (my-cond-branch 999) => 'other)

;; 测试5：递归语法宏
(define-syntax my-sum
  (syntax-rules ()
    ((my-sum) 0)
    ((my-sum x) x)
    ((my-sum x y ...) (+ x (my-sum y ...)))))

(check (my-sum) => 0)
(check (my-sum 5) => 5)
(check (my-sum 1 2 3 4) => 10)

(check-report)
