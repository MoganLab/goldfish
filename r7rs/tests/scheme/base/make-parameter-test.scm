;; make-parameter
;; 创建参数对象（parameter object）。
;;
;; 语法
;; ----
;; (make-parameter init)
;; (make-parameter init converter)
;;
;; 参数
;; ----
;; init : any
;; 参数的初始值。
;;
;; converter : procedure 可选
;; 单参数过程。init 与后续写入的新值都先经 converter 转换后再存储；
;; 省略时存储原值。
;;
;; 返回值
;; ----
;; procedure
;; 参数对象是过程：零参调用返回当前值；一参调用写入新值
;; （SRFI-39 扩展语义，parameterize 恢复旧值亦依赖于此）。
;;
;; 描述
;; ----
;; 1. 参数对象常与 parameterize 配合，实现动态作用域的参数传递。
;; 2. converter 在创建时作用于 init，此后每次写入新值时同样生效。
;;
;; 错误处理
;; --------
;; 无。
;;
;; (define p (make-parameter 10))
;; (p) => 10

(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; 初始值
(define p (make-parameter "initial value"))
(check (p) => "initial value")

;; 参数对象是过程
(check-true (procedure? p))

;; 省略 converter 时存储原值
(define num (make-parameter 42))
(check (num) => 42)

;; 带 converter：init 先经转换
(define str (make-parameter 1 (lambda (x) (number->string x))))
(check (str) => "1")

;; 写入新值同样经过 converter（SRFI-39 一参调用语义）
(str 2)
(check (str) => "2")

;; 一参调用写入、零参调用读取
(num 7)
(check (num) => 7)

;; 不同参数对象相互独立
(define a (make-parameter 'a))
(define b (make-parameter 'b))
(check (a) => 'a)
(check (b) => 'b)

(check-report)
