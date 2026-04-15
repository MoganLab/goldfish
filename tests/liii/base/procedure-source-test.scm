(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; procedure-source
;; 获取过程的源代码定义。
;;
;; 语法
;; ----
;; (procedure-source proc)
;;
;; 参数
;; ----
;; proc : procedure?
;; 要获取源代码的过程。
;;
;; 返回值
;; ------
;; list? 或 ()
;; 如果 proc 是 Scheme 定义的函数，返回 (lambda ...) 形式的列表；
;; 如果 proc 是 C 内置函数，返回空列表 ()。
;;
;; 说明
;; ----
;; procedure-source 可以获取用户定义的函数的源代码表示形式，
;; 这对于调试、代码分析和元编程非常有用。
;; 注意：C 内置函数没有源代码表示，因此返回空列表。


;; 测试 lambda 函数的源代码
(check (procedure-source (lambda (x) (+ x 1)))
  =>
  '(lambda (x) (+ x 1))
) ;check


;; 测试 define 定义的函数
(check (let ()
         (define (add1 x)
           (+ x 1)
         ) ;define
         (procedure-source add1)
       ) ;let
  =>
  '(lambda (x) (+ x 1))
) ;check


;; 测试 C 内置函数返回空列表
(check (procedure-source +) => '())


;; 测试多参数函数
(check (procedure-source (lambda (a b c) (+ a b c))
       ) ;procedure-source
  =>
  '(lambda (a b c) (+ a b c))
) ;check


;; 测试带默认参数的函数 (使用 define* 定义)
(check (let ()
         (define* (greet name (greeting "Hello"))
           (string-append greeting " " name)
         ) ;define*
         (procedure-source greet)
       ) ;let
  =>
  '(lambda* (name (greeting "Hello")) (string-append greeting " " name))
) ;check


(check-report)
