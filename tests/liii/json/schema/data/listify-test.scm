(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; listify
;; 把值规约为列表。
;;
;; 语法
;; ----
;; (listify value)
;;
;; 参数
;; ----
;; value : any?
;; 待规约的值。
;;
;; 返回值
;; ----
;; list
;; 向量转成列表，列表原样返回，原子值包装成单元素列表。
;;
;; 注意
;; ----
;; 空列表原样返回，空向量转成空列表。
;;
;; 示例
;; ----
;; (listify #(1 2)) => '(1 2)
;; (listify 1) => '(1)
;;
;; 错误处理
;; ----
;; 无。

(check (listify #(1 2)) => '(1 2))
(check (listify #()) => '())
(check (listify '(1 2)) => '(1 2))
(check (listify '()) => '())
(check (listify 1) => '(1))
(check (listify "x") => '("x"))
(check (listify 'null) => '(null))

(check-report)
