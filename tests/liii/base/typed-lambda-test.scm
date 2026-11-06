;; typed-lambda
;; 带类型标注的 lambda 语法，调用时校验参数类型。
;;
;; 语法
;; ----
;; (typed-lambda ((arg type) ...) body ...)
;; (typed-lambda (arg ...) body ...)
;;
;; 参数
;; ----
;; arg : symbol
;; 形参名称。
;;
;; type : procedure
;; 类型判定过程，接收一个值并返回布尔值。
;;
;; body : expression ...
;; 函数体。
;;
;; 返回值
;; ----
;; procedure
;; 返回带类型校验的新过程。
;;
;; 说明
;; ----
;; typed-lambda 在函数体执行前插入类型检查，
;; 形参类型不满足时抛出 type-error 错误，错误信息包含形参名与期望类型。
;;
;; 形参列表整体要再套一层括号：每个带类型的形参写成 (arg type)，
;; 形参列表形如 ((x number?) (y number?))。
;; 不带类型的形参可以直接写名字，与普通 lambda 一致。
;;
;; 错误处理
;; ----
;; type-error 当实参不满足对应类型的判定过程时抛出。

(import (liii check) (liii base))


(check-set-mode! 'report-failed)


;; 单个带类型的形参
(check ((typed-lambda ((x number?)) (* x 2)) 21) => 42)
(check ((typed-lambda ((x number?)) (* x 2)) 1.5) => 3.0)

;; 类型不匹配时抛出 type-error
(check-catch 'type-error ((typed-lambda ((x number?)) (* x 2)) "str"))
(check-catch 'type-error ((typed-lambda ((x string?)) string-length x) 42))

;; 多个带类型的形参
(check ((typed-lambda ((x number?) (y number?)) (+ x y)) 1 2) => 3)
(check-catch 'type-error ((typed-lambda ((x number?) (y number?)) (+ x y)) 1 "two"))

;; 混用带类型与不带类型的形参
(check ((typed-lambda ((x number?) y) (+ x y)) 1 2) => 3)
(check ((typed-lambda (x (y number?)) (+ x y)) 1 2) => 3)

;; 不带类型标注时行为与 lambda 一致
(check ((typed-lambda (x) (* x 3)) 4) => 12)


(check-report)
