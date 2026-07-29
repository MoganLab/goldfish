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
;; inlet 的绑定可以通过 set! 修改，有两种风格：
;;
;;   开放式：字段直接挂 inlet，外部用 (set! (obj :field) v) 改值。
;;           字段公开可见、可改，封装弱。
;;
;;   封装式：字段是工厂函数的闭包变量，inlet 只暴露读写方法。
;;           字段外部不可直接访问，封装强。


;; ---- 开放式：直接改 inlet 绑定 ----------------------------------
;; inlet 是可调用环境，(obj :key) 取值，set! 其返回位置即可改值

(define p1 (inlet :name "Bob" :age 25))

;; (p1 :age) 取值
(check (p1 :age) => 25)

;; set! 修改 :age 绑定
(check (begin (set! (p1 :age) 26) (p1 :age)) => 26)

;; 复合操作：age + 1
(check (begin (set! (p1 :age) (+ (p1 :age) 1)) (p1 :age)) => 27)


;; ---- 封装式：方法改闭包变量 -------------------------------------
;; age 是闭包变量，inlet 暴露 :get-age / :inc-age 方法
;; 调用时避免 ((obj :method) obj) 的冗余写法，
;; 定义顶层封装函数，调用处更自然

(define* (make-person (name "?") (age 0))
  (inlet :get-age
    (lambda (self) age)
    :inc-age
    (lambda (self) (set! age (+ age 1)) age)
  ) ;inlet
) ;define*

(define (get-age obj)
 ((obj :get-age) obj)
) ;define

(define (increase-age obj)
 ((obj :inc-age) obj)
) ;define

(define p2 (make-person :name "Bob" :age 25))

;; increase-age 修改 age，get-age 读取
(check (begin (increase-age p2) (get-age p2)) => 26)


(check-report)
