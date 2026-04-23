(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; procedure?
;; 判断对象是否为过程。
;;
;; 语法
;; ----
;; (procedure? obj)
;;
;; 参数
;; ----
;; obj : 任意类型
;; 待判断的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果 obj 是过程则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 内置过程和 lambda 表达式都返回 #t
;; 2. 宏返回 #f
(check (procedure? +) => #t)
(check (procedure? (lambda (x) x)) => #t)
(check (procedure? 1) => #f)
(check (procedure? 'sym) => #f)
(check (procedure? "str") => #f)
(check (procedure? #()) => #f)
(check (procedure? '()) => #f)
(check (procedure? #t) => #f)

(check-report)
