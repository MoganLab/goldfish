(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; eq?
;; 判断两个对象是否引用相同（对象为同一），即判断对象标识。
;;
;; 语法
;; ----
;; (eq? obj1 obj2)
;;
;; 参数
;; ----
;; obj1, obj2 : any
;; 任意类型的对象
;;
;; 返回值
;; -----
;; boolean?
;; 如果两个对象是同一对象则返回 #t，否则返回 #f。
;; Test eq? for boolean values
(check-true (eq? #t #t))
(check-true (eq? #f #f))
(check-false (eq? #t #f))
;; Test eq? for exact numbers (may return #f for different instances)
(check-true (eq? 42 42))
(check-false (eq? 42 43))
;; Test eq? for symbols
(check-true (eq? 'abc 'abc))
(check-false (eq? 'abc 'def))
;; Test eq? for lists (not the same instance)
(check-false (eq? (list 1 2 3) (list 1 2 3))
) ;check-false
(check-true (let ((lst (list 1 2 3)))
              (eq? lst lst)
            ) ;let
) ;check-true
;; Test eq? for strings (always #f due to different instances)
(check-false (eq? "hello" "hello"))
;; Test eq? for procedures
(check-true (eq? car car))
(check-false (eq? car cdr))
;; ; equal?
(check-report)
