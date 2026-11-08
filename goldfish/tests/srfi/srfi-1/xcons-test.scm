;; xcons
;; 交换参数顺序的cons操作。
;;
;; 语法
;; ----
;; (xcons obj1 obj2)
;;
;; 参数
;; ----
;; obj1 : any
;; 任意对象。
;; obj2 : any
;; 任意对象。
;;
;; 返回值
;; ------
;; pair
;; 返回 (obj2 . obj1) 组成的对。
;;
;; 错误处理
;; --------
;; - wrong-number-of-args：当参数数量不为 2 时抛出。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (xcons 1 2) => '(2 . 1))
(check (xcons 1 '(2 3)) => '((2 3) . 1))
(check (xcons '(1 2) 3) => '(3 1 2))
(check (xcons '(1 2) '(3 4)) => '((3 4) 1 2))
(check (xcons 1 '()) => '(() . 1))
(check (xcons '() 2) => '(2))
(check (xcons (xcons 1 2) 3) => '(3 2 . 1))

(check-catch 'wrong-number-of-args (xcons 1))
(check-catch 'wrong-number-of-args (xcons 1 2 3))

(check-report)
