(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector->vector
;; 将可变长向量转换为普通向量。
;;
;; 语法
;; ----
;; (flexvector->vector fv)
;; (flexvector->vector fv start)
;; (flexvector->vector fv start end)
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
;; vector
;; 转换后的普通向量。
;;
;; 描述
;; ----
;; 创建包含 flexvector 元素的新向量。

(let ((fv (flexvector 1 2 3)))
  (check (flexvector->vector fv) => #(1 2 3))
) ;let

(let ((fv (flexvector 'a 'b 'c 'd)))
  (check (flexvector->vector fv 1) => #(b c d))
  (check (flexvector->vector fv 1 3) => #(b c))
) ;let

(check (flexvector->vector (flexvector)) => #())

(check-report)
