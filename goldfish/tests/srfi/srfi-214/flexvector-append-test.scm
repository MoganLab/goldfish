;; flexvector-append
;; 连接可变长向量。
;;
;; 语法
;; ----
;; (flexvector-append fv ...)
;;
;; 参数
;; ----
;; fv : flexvector
;; 待拼接的可变长向量，至少传入一个。
;;
;; 返回值
;; ----
;; flexvector
;; 依次拼接各向量得到的新可变长向量。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(check (flexvector->vector (flexvector-append (flexvector 10 20) (flexvector) (flexvector 30 40))
       ) ;flexvector->vector
  =>
  #(10 20 30 40)
) ;check


(check-report)
