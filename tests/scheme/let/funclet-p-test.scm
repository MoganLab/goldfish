(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; funclet?
;; 判断对象是否为 funclet（函数闭包环境）。
;;
;; 语法
;; ----
;; (funclet? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要判断的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果 obj 是函数的闭包环境（funclet）则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; funclet? 专门判断 let 是否为某个函数的闭包环境。
;; 普通 inlet、curlet、rootlet 都不是 funclet。
;; 通过 (funclet proc) 获取的过程闭包环境是 funclet。


;; 普通 inlet 不是 funclet
(check (funclet? (inlet 'a 1)) => #f)


;; rootlet 不是 funclet
(check (funclet? (rootlet)) => #f)


;; curlet 在顶层不是 funclet
(check (funclet? (curlet)) => #f)


;; 顶层定义的、捕获了变量的闭包过程，其 funclet 是 funclet

(define make-adder (lambda (n) (lambda (x) (+ x n))))

(define add5 (make-adder 5))
(check (funclet? (funclet add5)) => #t)


;; 无闭包变量的 lambda，其 funclet 退化为 rootlet，不是 funclet
(check (funclet? (funclet (lambda (x) x))) => #f)


;; C 函数的 funclet 是 rootlet，不是 funclet
(check (funclet? (funclet car)) => #f)


;; 整数不是 funclet
(check (funclet? 42) => #f)


;; 字符串不是 funclet
(check (funclet? "hello") => #f)


(check-report)
