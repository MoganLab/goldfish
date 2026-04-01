(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-front
;; 访问可变长向量的首个元素。
;;
;; 语法
;; ----
;; (flexvector-front fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; 返回值
;; ----
;; any
;; 返回向量中的第一个元素。
;;
;; 错误处理
;; ----
;; bounds-error
;; 当向量为空时抛出。

(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-front fv) => 'a)
) ;let

(let ((fv (flexvector 10)))
  (check (flexvector-front fv) => 10)
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector-front fv) => 1)
) ;let

(check-report)
