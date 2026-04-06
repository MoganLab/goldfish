(import (liii check))
(import (liii base))

(check-set-mode! 'report-failed)

;; procedure-arglist
;; 获取过程的参数列表。
;;
;; 语法
;; ----
;; (procedure-arglist proc)
;;
;; 参数
;; ----
;; proc : procedure?
;; 要获取参数列表的 Scheme 函数。
;;
;; 返回值
;; ------
;; list? 或 symbol?
;; 返回函数的参数列表。对于普通函数返回列表；
;; 对于变参函数返回 (arg1 . rest) 形式的点对；
;; 对于只有一个 rest 参数的函数返回符号。
;;
;; 说明
;; ----
;; procedure-arglist 用于获取 Scheme 定义的函数的参数列表。
;; 注意：C 内置函数没有参数列表，调用会报错。

;; 测试固定参数 lambda
(check
  (procedure-arglist (lambda (x y) (+ x y)))
  => '(x y)
) ;check

;; 测试单参数函数
(check
  (procedure-arglist (lambda (x) x))
  => '(x)
) ;check

;; 测试无参数函数 (thunk)
(check
  (procedure-arglist (lambda () 42))
  => '()
) ;check

;; 测试变参函数
(check
  (procedure-arglist (lambda (x . rest) (cons x rest)))
  => '(x . rest)
) ;check

;; 测试只有 rest 参数的函数
(check
  (procedure-arglist (lambda args args))
  => 'args
) ;check

;; 测试 define 定义的函数
(check
  (let ()
    (define (add3 a b c) (+ a b c))
    (procedure-arglist add3)
  ) ;let
  => '(a b c)
) ;check

;; 测试 define* 定义的函数
(check
  (let ()
    (define* (greet name (greeting "Hello"))
      (string-append greeting " " name)
    ) ;define*
    (procedure-arglist greet)
  ) ;let
  => '(name (greeting "Hello"))
) ;check

(check-report)
