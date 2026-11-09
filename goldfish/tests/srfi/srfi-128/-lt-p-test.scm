;; <?
;; 使用比较器判断前一个值是否严格小于后一个值。
;;
;; 语法
;; ----
;; (<? comparator a b . objs)
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
;; a 按比较器严格小于 b 时返回 #t，否则返回 #f。
;; 注意
;; ----
;; 支持两个以上的比较对象，会依次两两比较。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check (<? (make-default-comparator) 1 2) => #t)
(check (<? (make-default-comparator) 2 1) => #f)
(check (<? (make-default-comparator) 1 1) => #f)

;; 多个对象
(check (<? (make-default-comparator) 1 2 3) => #t)
(check (<? (make-default-comparator) 1 3 2) => #f)

;; 自定义比较器
(let ((comp (make-comparator number? = < number-hash)))
  (check (<? comp 1 2) => #t)
  (check (<? comp 2 1) => #f)
) ;let

(check-report)
