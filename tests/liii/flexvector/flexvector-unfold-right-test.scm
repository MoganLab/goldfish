(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-unfold-right
;; 通过反向展开操作创建可变长向量。
;;
;; 语法
;; ----
;; (flexvector-unfold-right stop? mapper successor seed)
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
;; 反向展开结果向量。
;;
;; 描述
;; ----
;; 与 flexvector-unfold 类似，但结果向量中元素的顺序相反。

(check (flexvector->vector
         (flexvector-unfold-right (lambda (x) (> x 10))
                                  (lambda (x) (* x x))
                                  (lambda (x) (+ x 1))
                                  1)
         ) ;flexvector-unfold-right
       => #(100 81 64 49 36 25 16 9 4 1)
) ;check

(check (flexvector->vector
         (flexvector-unfold-right (lambda (x) (>= x 5))
                                  (lambda (x) x)
                                  (lambda (x) (+ x 1))
                                  0)
         ) ;flexvector-unfold-right
       => #(4 3 2 1 0)
) ;check

(check-report)
