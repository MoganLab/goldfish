(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-copy
;; 复制可变长向量。
;;
;; 语法
;; ----
;; (flexvector-copy fv)
;; (flexvector-copy fv start)
;; (flexvector-copy fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
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
;; 新的 flexvector 副本。
;;
;; 描述
;; ----
;; 创建一个新的 flexvector，包含源向量的元素。
;; 修改副本不会影响原向量。

(let ((fv (flexvector 1 2 3)))
  (let ((copy (flexvector-copy fv)))
    (check (flexvector-length fv) => (flexvector-length copy))
    (check-false (eq? fv copy))
    (check (flexvector-ref copy 0) => 1)
    (flexvector-set! copy 0 'x)
    (check (flexvector-ref fv 0) => 1)
    (check (flexvector-ref copy 0) => 'x)
  ) ;let
) ;let

(let ((fv (flexvector 'a 'b 'c 'd 'e)))
  (check (flexvector->vector (flexvector-copy fv 1 4)) => #(b c d))
) ;let

(check-report)
