(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; equal?
;; 判断两个对象结构是否相等，根据R7RS规范，equal?对复杂数据结构进行深比较。
;;
;; 语法
;; ----
;; (equal? obj1 obj2)
;;
;; 参数
;; ----
;; obj1, obj2 : any
;; 任意类型的对象
;;
;; 返回值
;; -----
;; boolean?
;; 如果两个对象结构相等则返回 #t，否则返回 #f。
;; Test equal? for simple types
(check-true (equal? #t #t))
(check-true (equal? 42 42))
(check-true (equal? 3.14 3.14))
(check-true (equal? "hello" "hello"))
(check-true (equal? 'abc 'abc))
;; Test equal? for lists
(check-true (equal? (list 1 2 3) (list 1 2 3))
) ;check-true
(check-false (equal? (list 1 2 3) (list 1 2 4))
) ;check-false
;; Test equal? for nested lists
(check-true (equal? (list (list 1 2) (list 3 4))
              (list (list 1 2) (list 3 4))
            ) ;equal?
) ;check-true
(check-false (equal? (list (list 1 2) (list 3 4))
               (list (list 1 2) (list 3 5))
             ) ;equal?
) ;check-false
;; Test equal? for vectors
(check-true (equal? (vector 1 2 3) (vector 1 2 3))
) ;check-true
(check-false (equal? (vector 1 2 3) (vector 1 2 4))
) ;check-false
;; Test equal? for nested vectors
(check-true (equal? (vector (vector 1 2) (vector 3 4))
              (vector (vector 1 2) (vector 3 4))
            ) ;equal?
) ;check-true
(check-false (equal? (vector (vector 1 2) (vector 3 4))
               (vector (vector 1 2) (vector 3 5))
             ) ;equal?
) ;check-false
;; Test equal? for mixed structures
(check-true (equal? (list 1 (vector 2 3) 4)
              (list 1 (vector 2 3) 4)
            ) ;equal?
) ;check-true
(check-false (equal? (list 1 (vector 2 3) 4)
               (list 1 (vector 2 4) 4)
             ) ;equal?
) ;check-false
;; Test equal? for empty structures
(check-true (equal? (list) (list)))
(check-true (equal? (vector) (vector)))
;; Test equal? for different types
(check-false (equal? 42 "hello"))
(check-false (equal? #\a "a"))
(check-report)
