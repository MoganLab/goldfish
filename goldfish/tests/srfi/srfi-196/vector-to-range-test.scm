;; vector->range
;; 将向量转换为 range（创建副本）。
;;
;; 语法
;; ----
;; (vector->range vec)
;;
;; 参数
;; ----
;; vec : vector
;; 源向量。
;;
;; 返回值
;; ----
;; range
;; 包含向量元素副本的 range 对象。
;;
;; 错误处理
;; ----
;; - type-error：当 vec 不是向量时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (vector->range #(1 2 3 4 5))))
  (check (range-length r) => 5)
  (check (range->list r) => '(1 2 3 4 5))
) ;let


(let ((r (vector->range #())))
  (check (range-length r) => 0)
  (check (range->list r) => '())
) ;let


(let ((r (vector->range #(a b c))))
  (check (range-length r) => 3)
  (check (range-ref r 0) => 'a)
  (check (range-ref r 2) => 'c)
) ;let

;; vec 不是向量时抛出 type-error
(check-catch 'type-error (vector->range "abc"))
(check-catch 'type-error (vector->range '(1 2 3)))
(check-catch 'type-error (vector->range 42))


(check-report)
