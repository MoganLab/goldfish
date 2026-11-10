;; =?
;; 使用比较器判断若干个值是否相等。
;;
;; 语法
;; ----
;; (=? comparator a b . objs)
;;
;; 参数
;; ----
;; comparator : comparator
;; a : any
;; b : any
;; objs : any ...
;; 可选的更多比较对象。
;; 返回值
;; ----
;; boolean
;; 所有值按比较器判定相等时返回 #t，否则返回 #f。
;; 注意
;; ----
;; 支持两个以上的比较对象，会依次两两比较。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


;; 相等
(check (=? (make-default-comparator) 1 1) => #t)
(check (=? (make-default-comparator) "a" "a") => #t)
(check (=? (make-default-comparator) 'sym 'sym) => #t)

;; 不等
(check (=? (make-default-comparator) 1 2) => #f)
(check (=? (make-default-comparator) "a" "b") => #f)

;; 三个及以上对象
(check (=? (make-default-comparator) 1 1 1) => #t)
(check (=? (make-default-comparator) 1 1 2) => #f)
(check (=? (make-default-comparator) 1 2 1) => #f)

;; 自定义比较器
(let ((comp (make-comparator number? = < number-hash)))
  (check (=? comp 1 1) => #t)
  (check (=? comp 1 2) => #f)
) ;let

(check-report)
