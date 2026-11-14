;; define*-record-type
;; 定义带有可选参数和关键字参数构造器的记录类型。
;;
;; 语法
;; ----
;; (define*-record-type type-name
;;   (constructor param ...)
;;   predicate
;;   (field-name accessor [modifier]) ...)
;;
;; 参数
;; ----
;; type-name : symbol
;; 记录类型名，统一采用 <xxx> 形状（如 <person>）。
;;
;; constructor : symbol
;; 构造函数名，由 define* 定义，支持位置参数与关键字参数。
;;
;; param : symbol 或 (symbol default)
;; 构造器参数名，可以带默认值表示可选参数。
;;
;; predicate : symbol
;; 类型判断函数名。
;;
;; field-name : symbol
;; 字段名。
;;
;; accessor : symbol
;; 字段访问函数名。
;;
;; modifier : symbol（可选）
;; 字段修改函数名。
;;
;; 返回值
;; ------
;; 无（定义类型谓词、支持关键字/默认值的构造函数、访问器与可选修改器）。

(import (liii check) (liii base))

(check-set-mode! 'report-failed)

;; 1. 必选参数 + 默认值 + 关键字参数 + 修改器
(define*-record-type <person>
  (make-person name (age 18) (role "user"))
  person?
  (name person-name set-person-name!)
  (age person-age)
  (role person-role)
) ;define*-record-type

(let ((p1 (make-person "Alice"))
      (p2 (make-person :role "admin" :name "Bob" :age 30))
      (p3 (make-person "Charlie" 25))
     ) ;
  (check-true (person? p1))
  (check-false (person? 'not-a-person))
  (check (person-name p1) => "Alice")
  (check (person-age p1) => 18)
  (check (person-role p1) => "user")
  (check (person-name p2) => "Bob")
  (check (person-age p2) => 30)
  (check (person-role p2) => "admin")
  (check (person-name p3) => "Charlie")
  (check (person-age p3) => 25)
  (check (person-role p3) => "user")
  (set-person-name! p1 "Alice2")
  (check (person-name p1) => "Alice2")
) ;let

;; 2. 全部参数都有默认值，可无参构造
(define*-record-type <config>
  (make-config (host "localhost") (port 8080) (tls? #f))
  config?
  (host config-host)
  (port config-port)
  (tls? config-tls?)
) ;define*-record-type

(let ((c0 (make-config))
      (c1 (make-config :port 9090 :tls? #t))
     ) ;
  (check-true (config? c0))
  (check (config-host c0) => "localhost")
  (check (config-port c0) => 8080)
  (check (config-tls? c0) => #f)
  (check (config-host c1) => "localhost")
  (check (config-port c1) => 9090)
  (check (config-tls? c1) => #t)
) ;let

;; 3. 默认值引用前面的参数，且构造器可只包含部分字段
(define*-record-type <rect>
  (make-rect w (h w))
  rect?
  (w rect-w)
  (h rect-h)
  (label rect-label set-rect-label!)
) ;define*-record-type

(let ((r1 (make-rect 10))
      (r2 (make-rect :h 20 :w 5))
     ) ;
  (check-true (rect? r1))
  (check (rect-w r1) => 10)
  (check (rect-h r1) => 10)
  (check (rect-w r2) => 5)
  (check (rect-h r2) => 20)
  (set-rect-label! r1 "square")
  (check (rect-label r1) => "square")
) ;let

;; 4. 无参数构造器
(define*-record-type <empty> (make-empty) empty?)

(check-true (empty? (make-empty)))

(check-report)
