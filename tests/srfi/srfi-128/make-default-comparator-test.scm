;; make-default-comparator
;; 创建默认比较器。
;;
;; 语法
;; ----
;; (make-default-comparator)
;;
;; 参数
;; ----

;; 返回值
;; ----
;; comparator
;; 返回默认比较器。
;; 注意
;; ----
;; 默认比较器按对象类型分派：先比较类型标签，再按类型比较。
;; 支持数字、字符串、字符、符号、布尔、列表、向量等内建类型。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let ((comp (make-default-comparator)))
  (check-true (comparator? comp))
  (check-true (comparator-ordered? comp))
  (check-true (comparator-hashable? comp))

  ;; 数值
  (check (<? comp 1 2) => #t)
  (check (<? comp 2 1) => #f)
  (check (=? comp 1 1) => #t)

  ;; 布尔
  (check (<? comp #f #t) => #t)
  (check (<? comp #t #f) => #f)

  ;; 字符、字符串与符号
  (check (<? comp #\a #\b) => #t)
  (check (<? comp "a" "b") => #t)
  (check (<? comp 'a 'b) => #t)

  ;; 列表与向量
  (check (<? comp '(1 2) '(2 3)) => #t)
  (check (<? comp #(1 2) #(2 3)) => #t)
  (check (<? comp '(1 2) '(1 3)) => #t)

  ;; 不同类型之间按类型标签排序
  (check (<? comp #f 1) => #t)
) ;let

(check-report)
