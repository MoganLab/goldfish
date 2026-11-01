;; syntax-error
;; 在宏展开期间显式报告语法错误并终止求值。
;;
;; 语法
;; ----
;; (syntax-error <message> <arg> ...)
;;
;; 参数
;; ----
;; <message> : string
;; 描述语法错误的提示信息。
;;
;; <arg> ... : any
;; 引起语法错误的表达式或相关对象。
;;
;; 返回值
;; -----
;; 不返回
;; 在展开期抛出受控的语法错误异常。

(import (scheme base) (srfi 78))

(check-set-mode! 'report-failed)

;; 测试1：展开期直接触发并捕获 syntax-error
(check (guard (e (else 'caught))
         (eval '(syntax-error "explicit syntax error"))
       ) ;guard
  =>
  'caught
) ;check

;; 测试2：在宏定义中按条件触发 syntax-error
(define-syntax check-sym
  (syntax-rules ()
    ((check-sym n) (syntax-error "expected a symbol" n))
  ) ;syntax-rules
) ;define-syntax

(check (guard (e (else 'caught-invalid-param))
         (eval '(check-sym 123))
       ) ;guard
  =>
  'caught-invalid-param
) ;check

;; 测试3：多参数语法错误
(check (guard (e (else 'caught-multi-arg))
         (eval '(syntax-error "bad form" 'a 'b 42))
       ) ;guard
  =>
  'caught-multi-arg
) ;check

(check-report)
