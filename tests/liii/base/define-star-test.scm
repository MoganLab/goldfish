(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; define*
;; 定义带有可选参数和关键字参数的函数。
;;
;; 语法
;; ----
;; (define* (name param ...) body ...)
;; (define* (name (param default) ...) body ...)
;; (define* (name (param default) ... . rest) body ...)
;;
;; 参数
;; ----
;; name : symbol
;; 函数名。
;;
;; param : symbol 或 (symbol default)
;; 参数名，可以带默认值表示可选参数。
;;
;; default : any
;; 可选参数的默认值。
;;
;; rest : symbol
;; 剩余参数列表。
;;
;; body ... : any
;; 函数体。
;;
;; 返回值
;; -----
;; procedure
;; 返回定义的函数。
;;
;; 说明
;; ----
;; define* 允许定义带有默认值的参数，调用时可以按位置传递或使用关键字传递。
;;
;; 限制
;; ----
;; 可选参数 (带默认值) 必须放在必需参数之后。
;; 如果可选参数需要出现在必需参数之前（如 range 函数），应使用 case-lambda：
;;   gf doc scheme/case-lambda


;; 基础测试 - 带有默认值的可选参数
(define* (hi a (b 32) (c "hi"))
  (list a b c)
) ;define*


;; 只提供必需参数
(check (hi 1) => '(1 32 "hi"))


;; 按关键字传递参数（可以打乱顺序）
(check (hi :b 2 :a 3) => '(3 2 "hi"))


;; 按位置传递所有参数
(check (hi 3 2 1) => '(3 2 1))


;; 参数默认值可以引用之前的参数
(define* (g a (b a) (k (* a b)))
  (list a b k)
) ;define*


(check (g 3 4) => '(3 4 12))


;; 使用关键字覆盖默认值
(check (g 3 4 :k 5) => '(3 4 5))


;; 混合使用位置和关键字参数
(define* (f x (y 10) (z 20)) (+ x y z))


(check (f 1) => 31)
(check (f 1 2) => 23)
(check (f 1 :z 100) => 111)
(check (f 1 2 :z 100) => 103)


(check-report)
