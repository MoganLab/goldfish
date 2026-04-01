(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; vector->flexvector
;; 将普通向量转换为可变长向量。
;;
;; 语法
;; ----
;; (vector->flexvector vec)
;; (vector->flexvector vec start)
;; (vector->flexvector vec start end)
;;
;; 参数
;; ----
;; vec : vector
;; 源向量。
;;
;; start : exact-nonnegative-integer (可选)
;; 起始索引，默认为 0。
;;
;; end : exact-nonnegative-integer (可选)
;; 结束索引，默认为向量长度。
;;
;; 返回值
;; ----
;; flexvector
;; 转换后的 flexvector。
;;
;; 描述
;; ----
;; 从普通向量创建 flexvector。

(check (flexvector->vector (vector->flexvector #(1 2 3))) => #(1 2 3))
(check (flexvector->vector (vector->flexvector #())) => #())

(let ((fv (vector->flexvector #(a b c d e) 1 4)))
  (check (flexvector->vector fv) => #(b c d))
) ;let

(check-report)
