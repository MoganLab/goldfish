(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-cumulate
;; 对可变长向量进行累积计算。
;;
;; 语法
;; ----
;; (flexvector-cumulate proc nil fv)
;;
;; 参数
;; ----
;; proc : function
;; 二元操作函数。
;;
;; nil : any
;; 初始值。
;;
;; fv : flexvector
;; 源向量。
;;
;; 返回值
;; ----
;; flexvector
;; 累积结果向量。
;;
;; 描述
;; ----
;; 返回一个新向量，其中每个元素是原向量从开始到当前位置的累积结果。

(check (flexvector->vector
         (flexvector-cumulate + 0 (flexvector 3 1 4 1 5 9 2 5 6)))
       => #(3 4 8 9 14 23 25 30 36)
) ;check

(check (flexvector->vector
         (flexvector-cumulate * 1 (flexvector 1 2 3 4 5)))
       => #(1 2 6 24 120)
) ;check

(check-report)
