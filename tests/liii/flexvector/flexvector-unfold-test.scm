(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-unfold
;; 通过展开操作创建可变长向量。
;;
;; 语法
;; ----
;; (flexvector-unfold stop? mapper successor seed)
;;
;; 参数
;; ----
;; stop? : function
;; 停止条件谓词。
;;
;; mapper : function
;; 将种子映射为元素的函数。
;;
;; successor : function
;; 计算下一个种子的函数。
;;
;; seed : any
;; 初始种子值。
;;
;; 返回值
;; ----
;; flexvector
;; 展开结果向量。
;;
;; 描述
;; ----
;; 类似于 SRFI-1 的 unfold，从种子值开始构建向量。

(check (flexvector->vector
         (flexvector-unfold (lambda (x) (> x 10))
                            (lambda (x) (* x x))
                            (lambda (x) (+ x 1))
                            1)
         ) ;flexvector-unfold
       => #(1 4 9 16 25 36 49 64 81 100)
) ;check

(check (flexvector->vector
         (flexvector-unfold (lambda (x) (>= x 5))
                            (lambda (x) x)
                            (lambda (x) (+ x 1))
                            0)
         ) ;flexvector-unfold
       => #(0 1 2 3 4)
) ;check

(check-report)
