(import (liii check))
(import (liii base))

(check-set-mode! 'report-failed)

;; arity
;; 获取过程可接受的参数数量范围。
;;
;; 语法
;; ----
;; (arity proc)
;;
;; 参数
;; ----
;; proc : any
;; 要检查的对象。
;;
;; 返回值
;; ------
;; pair? 或 #f
;; 如果对象是可应用的过程，返回 (min . max) 形式的数对；
;; min 是最小参数数量，max 是最大参数数量（536870912 表示无限制）。
;; 如果对象不可应用，返回 #f。
;;
;; 说明
;; ----
;; arity 用于查询过程可接受的参数数量范围。
;; 对于变参函数（使用 . rest），max 可能是一个很大的数表示无上限。

;; 测试固定参数 lambda
(check
  (arity (lambda (x y) (+ x y)))
  => '(2 . 2)
) ;check

;; 测试单参数函数
(check
  (arity (lambda (x) x))
  => '(1 . 1)
) ;check

;; 测试无参数函数 (thunk)
(check
  (arity (lambda () 42))
  => '(0 . 0)
) ;check

;; 测试变参函数
(check
  (arity (lambda (x . rest) (cons x rest)))
  => '(1 . 536870912)
) ;check

;; 测试只有 rest 参数的函数
(check
  (arity (lambda args args))
  => '(0 . 536870912)
) ;check

;; 测试 C 内置函数 +
(check
  (arity +)
  => '(0 . 536870912)
) ;check

;; 测试 C 内置函数 car (需要 1 个参数)
(check
  (arity car)
  => '(1 . 1)
) ;check

;; 测试 C 内置函数 cons (需要 2 个参数)
(check
  (arity cons)
  => '(2 . 2)
) ;check

;; 测试非可应用对象返回 #f
(check
  (arity 42)
  => #f
) ;check

;; 测试字符串 - 在 S7 中字符串可应用（作为一元函数）
(check
  (arity "hello")
  => '(1 . 1)
) ;check

;; 测试 define 定义的函数
(check
  (let ()
    (define (add3 a b c) (+ a b c))
    (arity add3)
  ) ;let
  => '(3 . 3)
) ;check

;; 测试 define* 定义的函数
;; 注意：define* 的所有参数都是可选的，所以 min 是 0
(check
  (let ()
    (define* (greet name (greeting "Hello"))
      (string-append greeting " " name)
    ) ;define*
    (arity greet)
  ) ;let
  => '(0 . 2)
) ;check

(check-report)
