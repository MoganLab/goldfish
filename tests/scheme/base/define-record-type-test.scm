(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; define-record-type
;; 定义一个新的记录类型，类似于其他语言中的结构体或类。
;;
;; 语法
;; ----
;; (define-record-type type-name
;;   (constructor field-name ...)
;;   predicate
;;   (field-name accessor [modifier]) ...)
;;
;; 参数
;; ----
;; type-name : symbol
;; 记录类型名，通常以冒号开头（如 :pare）。
;;
;; constructor : symbol
;; 构造函数名。
;;
;; field-name : symbol
;; 字段名。
;;
;; predicate : symbol
;; 类型判断函数名。
;;
;; accessor : symbol
;; 字段访问函数名。
;;
;; modifier : symbol (可选)
;; 字段修改函数名。
;;
;; 返回值
;; -----
;; 无（定义多个函数）。
;;
;; 说明
;; ----
;; define-record-type 创建一种新的数据类型，包含：
;; - 构造函数：用于创建记录实例
;; - 类型判断函数：用于判断是否为该类型的实例
;; - 访问函数：用于读取字段值
;; - 修改函数（可选）：用于修改字段值
;; 基础记录类型定义
(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr)
) ;define-record-type
;; 类型判断测试
(check (pare? (kons 1 2)) => #t)
(check (pare? (cons 1 2)) => #f)
;; 访问器测试
(check (kar (kons 1 2)) => 1)
(check (kdr (kons 1 2)) => 2)
;; 修改器测试
(check (let ((k (kons 1 2))) (set-kar! k 3) (kar k)) => 3)
;; 更实用的记录类型 - 人员信息
(define-record-type :person
  (make-person name age)
  person?
  (name get-name set-name!)
  (age get-age)
) ;define-record-type
(check (person? (make-person "Da" 3)) => #t)
(check (get-age (make-person "Da" 3)) => 3)
(check (get-name (make-person "Da" 3)) => "Da")
;; 使用修改器
(check (let ((da (make-person "Da" 3)))
         (set-name! da "Darcy")
         (get-name da)
       ) ;let
  =>
  "Darcy"
) ;check
;; 不可变字段尝试修改会导致错误
;; (set-age! da 4) 会报错，因为 age 没有定义修改器
;; 创建多个实例
(let ((p1 (make-person "Alice" 25)) (p2 (make-person "Bob" 30)))
  (check (get-name p1) => "Alice")
  (check (get-name p2) => "Bob")
  (check (+ (get-age p1) (get-age p2)) => 55)
) ;let
;; 构造器参数顺序与字段声明顺序不同
(define-record-type :point
  (make-point y x)
  point?
  (x point-x)
  (y point-y)
) ;define-record-type
(let ((p (make-point 10 20)))
  (check (point-y p) => 10)
  (check (point-x p) => 20)
) ;let
;; 构造器只初始化部分字段，其余字段默认 #f（Goldfish 行为）
(define-record-type :partial
  (make-partial a)
  partial?
  (a partial-a)
  (b partial-b)
) ;define-record-type
(let ((p (make-partial 1)))
  (check (partial-a p) => 1)
  (check (partial-b p) => #f)
) ;let
;; 构造器不含任何字段
(define-record-type :empty
  (make-empty)
  empty-rec?
  (a empty-a)
) ;define-record-type
(check (empty-rec? (make-empty)) => #t)
(check (empty-a (make-empty)) => #f)
;; 跨类型谓词判断
(check (pare? (make-person "Da" 3)) => #f)
(check (person? (kons 1 2)) => #f)
;; 谓词作用于非记录值
(check (pare? 3) => #f)
(check (pare? "abc") => #f)
(check (pare? #(1 2)) => #f)
(check (pare? (vector)) => #f)
(check (pare? '()) => #f)
;; 访问器/修改器作用于错误类型时报错
(check-catch 'wrong-type-arg (kar 3))
(check-catch 'wrong-type-arg (kar "abc"))
(check-catch 'wrong-type-arg (kar (cons 1 2)))
(check-catch 'wrong-type-arg (set-kar! (cons 1 2) 3))
;; 参数个数错误
(check-catch 'wrong-number-of-args (kons 1))
(check-catch 'wrong-number-of-args (kons 1 2 3))
(check-catch 'wrong-number-of-args (kar))
(check-catch 'wrong-number-of-args (set-kar! (kons 1 2)))
;; 在局部作用域中定义记录类型
(let ()
  (define-record-type :local
    (make-local v)
    local?
    (v local-v set-local-v!)
  ) ;define-record-type
  (let ((r (make-local 5)))
    (check (local? r) => #t)
    (check (local-v r) => 5)
    (set-local-v! r 6)
    (check (local-v r) => 6)
  ) ;let
) ;let
;; 字段值可以是另一个记录（嵌套记录）
(define-record-type :holder
  (make-holder content)
  holder?
  (content holder-content)
) ;define-record-type
(let ((h (make-holder (kons 1 2))))
  (check (holder? h) => #t)
  (check (pare? (holder-content h)) => #t)
  (check (kar (holder-content h)) => 1)
) ;let
;; 同名类型重复定义互不干扰（各自拥有独立的类型标识）
(let ()
  (define-record-type :dup
    (make-dup1 v)
    dup1?
    (v dup1-v)
  ) ;define-record-type
  (define-record-type :dup
    (make-dup2 v)
    dup2?
    (v dup2-v)
  ) ;define-record-type
  (check (dup1? (make-dup1 1)) => #t)
  (check (dup1? (make-dup2 1)) => #f)
  (check (dup2? (make-dup2 1)) => #t)
) ;let
(check-report)
