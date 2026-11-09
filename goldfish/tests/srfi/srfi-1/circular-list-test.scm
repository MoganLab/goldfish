;; circular-list
;; 构造一个包含给定元素的循环列表。
;;
;; 语法
;; ----
;; (circular-list obj1 obj2 ...)
;;
;; 参数
;; ----
;; obj1, obj2, ... : any
;; 任意数量的元素，至少需要一个元素。
;;
;; 返回值
;; ------
;; pair
;; 返回一个循环列表（circular list）。循环列表的最后一个元素的cdr指向列表的第一个元素，形成一个无限循环。
;;
;; 错误处理
;; --------
;; - wrong-number-of-args：当未提供参数时抛出。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check-true (circular-list? (circular-list 1)))
(check-true (circular-list? (circular-list 1 2)))
(check-true (circular-list? (circular-list 1 2 3)))

(let ((cl (circular-list 1 2 3)))
  (check (cl 0) => 1)
  (check (cl 1) => 2)
  (check (cl 2) => 3)
  (check (cl 3) => 1)
  (check (cl 4) => 2)
  (check (cl 5) => 3)
  (check (cl 6) => 1))

(check-true (circular-list? (circular-list 'a)))
(check-true (circular-list? (circular-list 'a 'b 'c)))
(check-true (circular-list? (circular-list "hello" "world")))
(check-true (circular-list? (circular-list '(1 2) '(3 4))))

(let ((single (circular-list 'x)))
  (check (single 0) => 'x)
  (check (single 1) => 'x)
  (check (single 100) => 'x))

(let ((nested (circular-list '(1 2) '(3) '(4 5 6))))
  (check (nested 0) => '(1 2))
  (check (nested 1) => '(3))
  (check (nested 2) => '(4 5 6))
  (check (nested 3) => '(1 2)))

(check-catch 'wrong-number-of-args (circular-list))

(check-report)
