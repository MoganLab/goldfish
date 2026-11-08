;; flexvector-concatenate
;; 连接可变长向量列表。
;;
;; 语法
;; ----
;; (flexvector-concatenate list)
;;
;; 参数
;; ----
;; list : list
;; 由可变长向量组成的列表。
;;
;; 返回值
;; ----
;; flexvector
;; 依次拼接列表中各向量得到的新可变长向量。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(check (flexvector->vector (flexvector-concatenate (list (flexvector 10 20) (flexvector) (flexvector 30 40))
                           ) ;flexvector-concatenate
       ) ;flexvector->vector
  =>
  #(10 20 30 40)
) ;check


(check-report)
