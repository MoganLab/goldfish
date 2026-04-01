(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-reverse!
;; 就地反转可变长向量。
;;
;; 语法
;; ----
;; (flexvector-reverse! fv)
;; (flexvector-reverse! fv start)
;; (flexvector-reverse! fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
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
;; 就地反转向量中指定范围的元素。

(let ((fv (flexvector 1 2 3)))
  (flexvector-reverse! fv)
  (check (flexvector->list fv) => '(3 2 1))
) ;let

(let ((fv (flexvector 'a 'b 'c 'd 'e)))
  (flexvector-reverse! fv 1 4)
  (check (flexvector->list fv) => '(a d c b e))
) ;let

(check-report)
