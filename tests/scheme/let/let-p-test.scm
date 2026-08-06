(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; let?
;; 判断对象是否为 let（环境对象）。
;;
;; 语法
;; ----
;; (let? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要判断的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果 obj 是 let 则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; let? 判断对象是否为环境（let）类型。
;; inlet、curlet、rootlet、owlet 等返回的对象都是 let。
;; funclet（函数闭包环境）也是 let。


;; curlet 返回的对象是 let
(check (let? (curlet)) => #t)


;; rootlet 是 let
(check (let? (rootlet)) => #t)


;; owlet 是 let
(check (let? (owlet)) => #t)


;; inlet 创建的对象是 let
(check (let? (inlet 'a 1)) => #t)


;; sublet 创建的对象是 let
(check (let? (sublet (inlet 'a 1) 'b 2)) => #t)


;; funclet 返回的对象是 let
(check (let? (funclet car)) => #t)


;; 整数不是 let
(check (let? 42) => #f)


;; 字符串不是 let
(check (let? "hello") => #f)


;; 符号不是 let
(check (let? 'sym) => #f)


;; 列表不是 let
(check (let? (list 1 2 3)) => #f)


;; #t 不是 let
(check (let? #t) => #f)


;; '() 不是 let
(check (let? '()) => #f)


(check-report)
