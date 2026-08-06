(import (liii check))
(import (scheme let))
(import (liii json))


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
;; inlet 是构造 let（环境对象）的基本方式。
;; 无参数调用 (inlet) 创建空 let。
;; (inlet 'a 1 'b 2) 创建包含 a=1、b=2 的 let。
;; inlet 创建的 let 是一个独立环境，其 outlet 是 rootlet。
;;
;; inlet 是可变的：通过 (set! (obj :key) v) 修改内部绑定。
;; 关键性质：set! 只改绑定值，不创建新对象——对象身份（eq?）恒定不变。
;; 因此 inlet 适合做需要就地更新、身份稳定的数据载体（如 data class）。
;;
;; 示例
;; ----
;; (define p (inlet :name "Bob" :age 25))
;; (define alias p)
;; (set! (p :age) 26)        ; 改 :age 绑定
;; (eq? p alias)             ; => #t，对象身份没变
;; (alias :age)              ; => 26，别名看到新值


;; inlet 是可变环境：set! 改绑定值，但对象身份不变（核心不变式）
(check (let* ((p (inlet :name "Bob" :age 25)) (alias p))
         (set! (p :age) 26)
         (list (eq? p alias) (alias :age))
       ) ;let*
  =>
  '(#t 26)
) ;check


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


;; ---- data class：inlet + 顶层序列化函数 ---------------------------
;; person data class 的正确做法：对象就是 inlet，字段就是绑定，
;; 序列化/反序列化用顶层函数，无需 openlet、无需闭包封装。
;; 字段用 keyword 键；序列化为 (liii json) 兼容的字符串键 alist。

(define (make-person name age)
  (inlet :name name :age age)
) ;define

(define (person->json p)
  (list (cons "name" (p :name)) (cons "age" (p :age)))
) ;define

(define (json->person j)
  (make-person (json-ref j "name") (json-ref j "age"))
) ;define

(define (string->person s)
  (json->person (string->json s))
) ;define


;; 字段访问
(check (let ((p (make-person "Bob" 25)))
         (list (p :name) (p :age))
       ) ;let
  =>
  '("Bob" 25)
) ;check

;; 序列化：person -> alist -> JSON 字符串
(check (let ((p (make-person "Bob" 25)))
         (person->json p)
       ) ;let
  =>
  '(("name" . "Bob") ("age" . 25))
) ;check

;; 反序列化：JSON 字符串 -> person
(check (let ((p (string->person "{\"name\":\"Alice\",\"age\":30}")))
         (list (p :name) (p :age))
       ) ;let
  =>
  '("Alice" 30)
) ;check

;; 完整往返：JSON -> person -> JSON
(check (let ((p (string->person "{\"name\":\"Alice\",\"age\":30}")))
         (json->string (person->json p))
       ) ;let
  =>
  "{\"name\":\"Alice\",\"age\":30}"
) ;check


(check-report)
