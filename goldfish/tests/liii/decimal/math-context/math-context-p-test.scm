;; math-context?
;; 判断对象是否为 math-context。
;;
;; 语法
;; ----
;; (math-context? obj)
;;
;; 参数
;; ----
;; obj : any
;;   任意对象。
;;
;; 返回值
;; ----
;; boolean?
;;   如果是 math-context 返回 #t，否则返回 #f。

(import (liii check)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(check (math-context? (make-math-context 10 'half-up)) => #t)
(check (math-context? 'half-up) => #f)
(check (math-context? 10) => #f)
(check (math-context? "precision") => #f)
(check (math-context? math-context-unlimited) => #t)

(check-report)
