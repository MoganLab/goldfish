(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-concatenate
;; 连接可变长向量列表。
;;
;; 语法
;; ----
;; (flexvector-concatenate fv-list)
;;
;; 参数
;; ----
;; fv-list : list
;; flexvector 的列表。
;;
;; 返回值
;; ----
;; flexvector
;; 包含所有向量元素的新向量。
;;
;; 描述
;; ----
;; 将 flexvector 列表中的所有向量连接成一个新向量。

(check (flexvector->vector
         (flexvector-concatenate
           (list (flexvector 10 20) (flexvector) (flexvector 30 40)))
         ) ;flexvector-concatenate
       => #(10 20 30 40)
) ;check

(check (flexvector->vector
         (flexvector-concatenate (list (flexvector 1 2) (flexvector 3 4))))
       => #(1 2 3 4)
) ;check

(check-report)
