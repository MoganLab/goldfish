;; flexvector->string
;; 可变长向量转换为字符串。
;;
;; 语法
;; ----
;; (flexvector->string fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 待转换的可变长向量。
;;
;; 返回值
;; ----
;; string
;; 由元素组成的字符串。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(check (flexvector->string (flexvector #\a #\b #\c)) => "abc")


(check-report)
