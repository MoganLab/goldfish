;;
;; iset-any?
;; 检查集合中是否有元素满足谓词。
;;

;; 语法
;; ----
;; (iset-any? predicate iset)
;;

;; 参数
;; ----
;; pred : procedure
;; 接受一个元素并返回布尔值的判定过程。
;;
;; set : iset
;; 待检查的集合。

;; 返回值
;; ----
;; 如果至少有一个元素满足谓词，返回 #t；否则返回 #f。
;; 注意：不同于 SRFI 1 的 any，此函数不返回满足谓词的元素。
;;

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(check-true (iset-any? positive? (iset -2 -1 1 2)))
(check-false (iset-any? zero? (iset -2 -1 1 2)))
(check-false (iset-any? even? (iset)))


(check-report)
