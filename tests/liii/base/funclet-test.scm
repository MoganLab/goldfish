(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; funclet
;; 返回过程的闭包环境（funclet）。
;;
;; 语法
;; ----
;; (funclet proc)
;;
;; 参数
;; ----
;; proc : procedure?
;; 过程（函数）。
;;
;; 返回值
;; ------
;; let?
;; 该过程的闭包环境。
;;
;; 说明
;; ----
;; funclet 返回过程定义时所处的词法环境。
;; 带捕获变量的闭包过程，其 funclet 包含被捕获的变量绑定。
;; 顶层 define 的过程，其 funclet 还包含其参数槽。
;; C 内建函数（如 car）的 funclet 为 rootlet。


;; funclet 返回 let
(check (let? (funclet car)) => #t)


;; C 内建函数的 funclet 是 rootlet
(check (eq? (funclet car) (rootlet)) => #t)


;; 带闭包变量的过程，其 funclet 包含捕获的变量

(define make-adder (lambda (n) (lambda (x) (+ x n))))

(define add5 (make-adder 5))
(check (let-ref (funclet add5) 'n) => 5)


;; 带闭包的 funclet 是 funclet?
(check (funclet? (funclet add5)) => #t)


;; 顶层 define 的简单过程，其 funclet 包含参数槽

(define simple-f (lambda (x) x))
(check (let->list (funclet simple-f)) => (list (list 'x)))


;; 同一过程的 funclet 多次获取得到同一对象
(check (eq? (funclet add5) (funclet add5)) => #t)


(check-report)
