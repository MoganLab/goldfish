(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; eqv?
;; 判断两个对象是否值相等，根据R7RS规范，eqv?在不同类型的数据上表现不同。
;;
;; 语法
;; ----
;; (eqv? obj1 obj2)
;;
;; 参数
;; ----
;; obj1, obj2 : any
;; 任意类型的对象
;;
;; 返回值
;; -----
;; boolean?
;; 如果两个对象值相等则返回 #t，否则返回 #f。
;; Test eqv? for boolean values
(check-true (eqv? #t #t))
(check-true (eqv? #f #f))
(check-false (eqv? #t #f))
;; Test eqv? for exact numbers
(check-true (eqv? 42 42))
(check-false (eqv? 42 43))
;; Test eqv? for inexact numbers
(check-true (eqv? 3.14 3.14))
(check-false (eqv? 3.14 2.71))
;; Test eqv? for characters
(check-true (eqv? #\a #\a))
(check-false (eqv? #\a #\b))
;; Test eqv? for symbols
(check-true (eqv? 'abc 'abc))
(check-false (eqv? 'abc 'def))
;; Test eqv? for lists (same instance)
(check-true (let ((lst (list 1 2 3))) (eqv? lst lst)))
;; Test eqv? for lists (different instances)
(check-false (eqv? (list 1 2 3) (list 1 2 3)))
;; Test eqv? for strings (always #f due to different instances)
(check-false (eqv? "hello" "hello"))
(check-false (eqv? "hello" "world"))
;; Test eqv? for procedures
(check-true (eqv? car car))
(check-false (eqv? car cdr))
;; ; eq?
(check-report)