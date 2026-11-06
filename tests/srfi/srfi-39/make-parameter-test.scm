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
;; 转换过程，接收新值并返回实际生效的值；省略时表示不做转换。
;;
;; 返回值
;; ----
;; procedure
;; 返回一个零参数过程，调用时返回参数当前的值。
;;
;; 说明
;; ----
;; 参数对象常与 parameterize 配合使用：
;; parameterize 在动态作用域内临时替换参数值，退出后自动恢复。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-39))


(check-set-mode! 'report-failed)


;; 初始值
(define mp (make-parameter "initial value"))
(check (mp) => "initial value")

;; 参数对象是过程
(check-true (procedure? mp))

;; 省略转换过程时直接返回原值
(define num (make-parameter 42))
(check (num) => 42)

;; 带转换过程：写入值时先经过转换
(define str (make-parameter 1 (lambda (x) (number->string x))))
(check (str) => "1")

;; 不同参数对象相互独立
(define a (make-parameter 'a))
(define b (make-parameter 'b))
(check (a) => 'a)
(check (b) => 'b)


(check-report)
