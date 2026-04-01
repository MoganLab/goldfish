(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-append!
;; 就地连接可变长向量。
;;
;; 语法
;; ----
;; (flexvector-append! fv1 fv2 ...)
;;
;; 参数
;; ----
;; fv1 : flexvector
;; 目标向量，将被修改。
;;
;; fv2, ... : flexvector
;; 要追加的向量。
;;
;; 返回值
;; ----
;; flexvector
;; 返回修改后的 fv1。
;;
;; 描述
;; ----
;; 将其他向量的元素追加到第一个向量中。

(let ((fv (flexvector 10 20)))
  (flexvector-append! fv (flexvector 30 40))
  (check (flexvector->vector fv) => #(10 20 30 40))
) ;let

(let ((fv (flexvector 1)))
  (flexvector-append! fv (flexvector 2) (flexvector 3 4))
  (check (flexvector->list fv) => '(1 2 3 4))
) ;let

(check-report)
