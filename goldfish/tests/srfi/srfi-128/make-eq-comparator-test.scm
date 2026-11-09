;; make-eq-comparator
;; 创建以 eq? 判定相等的比较器。
;;
;; 语法
;; ----
;; (make-eq-comparator)
;;
;; 参数
;; ----

;; 返回值
;; ----
;; comparator
;; 返回使用 eq? 判定相等的比较器。
;; 注意
;; ----
;; 该比较器不支持排序。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let ((comp (make-eq-comparator)))
  (check-true (comparator? comp))
  (check-false (comparator-ordered? comp))
  (check (=? comp 'a 'a) => #t)
  (check (=? comp 'a 'b) => #f)
  (check (=? comp "x" "x") => #f)
) ;let

(check-report)
