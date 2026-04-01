(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-fill!
;; 填充可变长向量。
;;
;; 语法
;; ----
;; (flexvector-fill! fv fill)
;; (flexvector-fill! fv fill start)
;; (flexvector-fill! fv fill start end)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; fill : any
;; 填充值。
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
;; 返回修改后的向量。
;;
;; 描述
;; ----
;; 将指定范围内的所有元素设置为相同的值。

(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'x)
  (check (flexvector->list fv) => '(x x x x x))
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'y 2)
  (check (flexvector->list fv) => '(1 2 y y y))
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'z 1 3)
  (check (flexvector->list fv) => '(1 z z 4 5))
) ;let

(check-report)
