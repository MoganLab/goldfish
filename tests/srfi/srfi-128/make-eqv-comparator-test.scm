;; make-eqv-comparator
;; 创建以 eqv? 判定相等的比较器。
;;
;; 语法
;; ----
;; (make-eqv-comparator)
;;
;; 参数
;; ----

;; 返回值
;; ----
;; comparator
;; 返回使用 eqv? 判定相等的比较器。
;; 注意
;; ----
;; 该比较器不支持排序。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let ((comp (make-eqv-comparator)))
  (check-true (comparator? comp))
  (check-false (comparator-ordered? comp))
  (check (=? comp 1 1) => #t)
  (check (=? comp 1 2) => #f)
  (check (=? comp 1.5 1.5) => #t)
) ;let

(check-report)
