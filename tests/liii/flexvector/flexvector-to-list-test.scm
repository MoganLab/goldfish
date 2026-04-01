(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector->list
;; 将可变长向量转换为列表。
;;
;; 语法
;; ----
;; (flexvector->list fv)
;; (flexvector->list fv start)
;; (flexvector->list fv start end)
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
;; list
;; 转换后的列表。
;;
;; 描述
;; ----
;; 创建包含 flexvector 元素的新列表。

(let ((fv (flexvector 1 2 3)))
  (check (flexvector->list fv) => '(1 2 3))
) ;let

(let ((fv (flexvector 'a 'b 'c 'd)))
  (check (flexvector->list fv 1) => '(b c d))
  (check (flexvector->list fv 1 3) => '(b c))
) ;let

(check (flexvector->list (flexvector)) => '())

(check-report)
