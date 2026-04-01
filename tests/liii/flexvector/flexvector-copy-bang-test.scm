(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-copy!
;; 将源向量的内容复制到目标向量。
;;
;; 语法
;; ----
;; (flexvector-copy! to at from)
;; (flexvector-copy! to at from start)
;; (flexvector-copy! to at from start end)
;;
;; 参数
;; ----
;; to : flexvector
;; 目标向量。
;;
;; at : exact-nonnegative-integer
;; 目标向量的起始位置。
;;
;; from : flexvector
;; 源向量。
;;
;; start : exact-nonnegative-integer (可选)
;; 源向量的起始索引。
;;
;; end : exact-nonnegative-integer (可选)
;; 源向量的结束索引。
;;
;; 返回值
;; ----
;; flexvector
;; 返回修改后的目标向量。
;;
;; 描述
;; ----
;; 将源向量的元素复制到目标向量的指定位置。

(let ((to (flexvector 1 2 3 4 5))
      (from (flexvector 20 30 40)))
  (flexvector-copy! to 1 from)
  (check (flexvector->list to) => '(1 20 30 40 5))
) ;let

(let ((to (flexvector 1 2 3 4 5))
      (from (flexvector 10 20 30 40 50)))
  (flexvector-copy! to 1 from 1 4)
  (check (flexvector->list to) => '(1 20 30 40 5))
) ;let

(check-report)
