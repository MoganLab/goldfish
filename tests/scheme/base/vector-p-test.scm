(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector?
;; 判断对象是否为向量。
;;
;; 语法
;; ----
;; (vector? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果对象是向量则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 空向量也是向量
;; 2. 列表不是向量
(check (vector? #(1 2 3)) => #t)
(check (vector? #()) => #t)
(check (vector? '(1 2 3)) => #f)
(check (vector? "abc") => #f)
(check (vector? 123) => #f)
(check (vector? 'vector) => #f)
(check-catch 'wrong-number-of-args (vector?))
(check-catch 'wrong-number-of-args (vector? 1 2))

(check-report)
