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
  (y kdr))

;; 类型判断测试
(check (pare? (kons 1 2)) => #t)
(check (pare? (cons 1 2)) => #f)

;; 访问器测试
(check (kar (kons 1 2)) => 1)
(check (kdr (kons 1 2)) => 2)

;; 修改器测试
(check
  (let ((k (kons 1 2)))
    (set-kar! k 3)
    (kar k))
  => 3)

;; 更实用的记录类型 - 人员信息
(define-record-type :person
  (make-person name age)
  person?
  (name get-name set-name!)
  (age get-age))

(check (person? (make-person "Da" 3)) => #t)
(check (get-age (make-person "Da" 3)) => 3)
(check (get-name (make-person "Da" 3)) => "Da")

;; 使用修改器
(check
  (let ((da (make-person "Da" 3)))
    (set-name! da "Darcy")
    (get-name da))
  => "Darcy")

;; 不可变字段尝试修改会导致错误
;; (set-age! da 4) 会报错，因为 age 没有定义修改器

;; 创建多个实例
(let ((p1 (make-person "Alice" 25))
      (p2 (make-person "Bob" 30)))
  (check (get-name p1) => "Alice")
  (check (get-name p2) => "Bob")
  (check (+ (get-age p1) (get-age p2)) => 55))

(check-report)
