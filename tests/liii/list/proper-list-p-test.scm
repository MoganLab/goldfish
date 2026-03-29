(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; proper-list?
;; 判断一个对象是否为proper list。
;;
;; 语法
;; ----
;; (proper-list? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象
;;
;; 返回值
;; ----
;; boolean?
;; 如果obj是proper list返回#t，否则返回#f。
;;
;; 说明
;; ----
;; proper list是指一个符合R7RS规范的传统列表结构，满足以下条件：
;; 1. 空列表'()是proper list
;; 2. 通过cons操作递归构建的以空列表结尾的列表是proper list
;; 3. 不包含循环引用的列表
;;
;; 非proper list的情况包括：
;; - 点对 (a . b)
;; - dotted list (a b . c)
;; - 循环列表
;; - 非pair和null的对象
;;
;; 使用场景
;; --------
;; 该函数常用于类型检查，确保输入参数符合列表操作的要求，避免在列表操作函数中发生类型错误。
;;
;; 错误处理
;; --------
;; 无特殊错误处理，任何类型的对象都能接受。

; 基本功能测试
(check-true (proper-list? (list 1 2)))
(check-true (proper-list? '()))
(check-true (proper-list? '(1 2 3)))

; 非proper list测试
(check-false (proper-list? '(a . b)))
(check-false (proper-list? '(a b . c)))
(check-false (proper-list? (circular-list 1 2 3)))

; 边界条件测试
(check-true (proper-list? '(() ()) ))
(check-true (proper-list? '(a)))
(check-false (proper-list? 1))
(check-false (proper-list? 'hello))
(check-false (proper-list? "hello"))

; 复杂结构测试
(check-false (proper-list? '(a b . c)))
(check-true (proper-list? '(a b (c d))))
(check-true (proper-list? '(() a b)))
(check-true (proper-list? '(a b ())))

; 点和dotted list测试
(check-false (proper-list? '(a . b)))
(check-false (proper-list? '(a b . c)))
(check-false (proper-list? '(a b c . d)))

; 循环列表测试
(let ((lst (list 1 2 3)))
  (set-cdr! (last-pair lst) lst)
  (check-false (proper-list? lst))
) ;let

(check-report)
