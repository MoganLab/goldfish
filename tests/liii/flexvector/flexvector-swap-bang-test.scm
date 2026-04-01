(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-swap!
;; 交换可变长向量中两个位置的元素。
;;
;; 语法
;; ----
;; (flexvector-swap! fv i j)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; i : exact-nonnegative-integer
;; 第一个索引。
;;
;; j : exact-nonnegative-integer
;; 第二个索引。
;;
;; 返回值
;; ----
;; flexvector
;; 返回修改后的向量。
;;
;; 描述
;; ----
;; 交换向量中索引 i 和 j 处的元素。

(let ((fv (flexvector 10 20 30)))
  (flexvector-swap! fv 0 2)
  (check (flexvector->list fv) => '(30 20 10))
) ;let

(let ((fv (flexvector 'a 'b 'c 'd)))
  (flexvector-swap! fv 1 2)
  (check (flexvector->list fv) => '(a c b d))
) ;let

(check-report)
