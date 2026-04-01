(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-append
;; 连接多个可变长向量。
;;
;; 语法
;; ----
;; (flexvector-append fv ...)
;;
;; 参数
;; ----
;; fv ... : flexvector
;; 要连接的向量。
;;
;; 返回值
;; ----
;; flexvector
;; 包含所有向量元素的新向量。
;;
;; 描述
;; ----
;; 将多个 flexvector 连接成一个新向量。

(check (flexvector->vector
         (flexvector-append (flexvector 10 20) (flexvector) (flexvector 30 40)))
       => #(10 20 30 40)
) ;check

(check (flexvector->vector (flexvector-append (flexvector 1 2) (flexvector 3 4)))
       => #(1 2 3 4)
) ;check

(check (flexvector->vector (flexvector-append (flexvector))) => #())

(check-report)
