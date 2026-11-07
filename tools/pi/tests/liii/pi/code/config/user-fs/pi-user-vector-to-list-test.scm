;; pi-user-vector->list
;; 将 JSON 解出的向量转换为列表。
;;
;; 语法
;; ----
;; (pi-user-vector->list v)
;;
;; 参数
;; ----
;; v : any
;; 待转换的对象。
;;
;; 返回值
;; ----
;; any
;; 向量转为列表，非向量原样返回。

(import (liii check)
  (liii pi code config user-fs)
) ;import

(check-set-mode! 'report-failed)

;; 1. 向量转为列表
(check (pi-user-vector->list (vector 1 2 3)) => '(1 2 3))

;; 2. 非向量原样返回
(check (pi-user-vector->list '(1 2)) => '(1 2))
(check (pi-user-vector->list "text") => "text")

(check-report)
