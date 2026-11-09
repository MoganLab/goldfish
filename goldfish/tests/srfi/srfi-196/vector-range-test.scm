;; vector-range
;; 从向量创建 range。
;;
;; 语法
;; ----
;; (vector-range vec)
;;
;; 参数
;; ----
;; vec : vector
;; 源向量。
;;
;; 返回值
;; ----
;; range
;; 包含向量元素的 range 对象。
;;
;; 注意
;; ----
;; 创建的 range 与源向量共享元素。
;;
;; 错误处理
;; ----
;; - type-error：当 vec 不是向量时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (vector-range #(a b c d e))))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 'a)
  (check (range-ref r 4) => 'e)
) ;let

;; 空 向量 合法
(check (range-length (vector-range #())) => 0)

;; vec 不是向量时抛出 type-error
(check-catch 'type-error (vector-range "abc"))
(check-catch 'type-error (vector-range '(a b c)))
(check-catch 'type-error (vector-range 42))
(check-catch 'type-error (vector-range 'vec))


(check-report)
