;;
;; iset-fold-right
;; 对集合进行折叠操作（按递减顺序）。
;;

;; 语法
;; ----
;; (iset-fold-right proc nil iset)
;;

;; 参数
;; ----
;; proc : procedure
;; 接受元素和累积值，返回新累积值的函数。
;;
;; nil : any
;; 初始累积值。
;;
;; iset : iset
;; 目标集合。
;;

;; 返回值
;; ----
;; any
;; 返回折叠后的累积值。

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(check (iset-fold-right cons '() (iset 2 3 5 7 11)) => '(2 3 5 7 11))


(check-report)
