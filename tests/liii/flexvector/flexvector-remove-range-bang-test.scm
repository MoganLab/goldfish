(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-remove-range!
;; 移除指定范围内的元素。
;;
;; 语法
;; ----
;; (flexvector-remove-range! fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; start : exact-nonnegative-integer
;; 起始索引（包含）。
;;
;; end : exact-nonnegative-integer
;; 结束索引（不包含）。
;;
;; 返回值
;; ----
;; flexvector
;; 返回修改后的 flexvector。
;;
;; 描述
;; ----
;; 移除从 start 到 end-1 的所有元素。

(let ((fv (flexvector 'a 'b 'c 'd 'e 'f)))
  (flexvector-remove-range! fv 1 4)
  (check (flexvector->list fv) => '(a e f))
) ;let

(let ((fv (flexvector 'a 'b 'c 'd 'e 'f)))
  (flexvector-remove-range! fv 1 1)
  (check (flexvector->list fv) => '(a b c d e f))
) ;let

(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-remove-range! fv 0 3)
  (check (flexvector-empty? fv) => #t)
) ;let

(check-report)
