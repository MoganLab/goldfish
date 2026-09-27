(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; context?
;; 判断对象是否为 context。
;;
;; 语法
;; ----
;; (context? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 是 context 返回 #t，否则返回 #f。

(check (context? (make-context)) => #t)
(check (context? (make-chan)) => #f)
(check (context? 42) => #f)

(check-report)
