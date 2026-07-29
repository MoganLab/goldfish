(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; inlet
;; 创建一个新的 let（环境对象），可带初始绑定。
;;
;; 语法
;; ----
;; (inlet)
;; (inlet symbol value ...)
;;
;; 参数
;; ----
;; symbol : symbol?
;; 绑定名。
;;
;; value : any
;; 绑定值。
;;
;; 返回值
;; ------
;; let?
;; 新创建的 let。
;;
;; 说明
;; ----
;; inlet 是构造 let 的基本方式。
;; 无参数调用 (inlet) 创建空 let。
;; (inlet 'a 1 'b 2) 创建包含 a=1、b=2 的 let。
;; inlet 创建的 let 是一个独立环境，其 outlet 是 rootlet。


;; 空 inlet 是 let
(check (let? (inlet)) => #t)


;; 带绑定的 inlet
(check (let? (inlet 'a 1)) => #t)


;; inlet 创建的 let 可以通过 let-ref 访问绑定
(check (let-ref (inlet 'a 42) 'a) => 42)


;; 多个绑定的 inlet
(check (let-ref (inlet 'a 1 'b 2 'c 3) 'b) => 2)


;; inlet 的长度反映绑定数量
(check (length (inlet 'a 1 'b 2)) => 2)


;; inlet 的 outlet 是 rootlet
(check (eq? (outlet (inlet 'a 1)) (rootlet)) => #t)


;; 两次 inlet 创建不同的 let 对象
(check (eq? (inlet 'a 1) (inlet 'a 1)) => #f)


;; 空 inlet 长度为 0
(check (length (inlet)) => 0)


;; =========================================================================
;; inlet 是可变的
;; =========================================================================
;;
;; inlet 是可调用环境，(obj :key) 取值，set! 其返回位置即可改值。


;; ---- inlet 是可变的：直接改绑定 ----------------------------------

(define p1 (inlet :name "Bob" :age 25))

;; (p1 :age) 取值
(check (p1 :age) => 25)

;; set! 修改 :age 绑定
(check (begin (set! (p1 :age) 26) (p1 :age)) => 26)

;; 复合操作：age + 1
(check (begin (set! (p1 :age) (+ (p1 :age) 1)) (p1 :age)) => 27)


;; ---- 顶层函数封装 -------------------------------------------------
;; 复合操作写多遍会啰嗦，封装成 person-age / person-inc-age! 等顶层函数，
;; 内部仍操作 inlet 绑定，对象本身保持简单

(define (person-age p)
  (p :age)
) ;define

(define (person-set-age! p v)
  (set! (p :age) v)
) ;define

(define (person-inc-age! p)
  (person-set-age! p (+ (person-age p) 1))
) ;define

(define p2 (inlet :name "Alice" :age 30))

;; 通过封装函数读写 age
(check (person-age p2) => 30)

(check (begin (person-inc-age! p2) (person-age p2)) => 31)


(check-report)
