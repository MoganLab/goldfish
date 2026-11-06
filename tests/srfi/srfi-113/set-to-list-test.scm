;; set->list
;; 将 set 转换为列表（顺序未指定）。
;;
;; 语法
;; ----
;; (set->list set)
;;
;; 参数
;; ----
;; set : set
;; 源 set。
;;
;; 返回值
;; ------
;; list
;; 返回包含 set 元素的新列表。
;;
;; 注意
;; ----
;; 元素顺序未指定。
;;


;; Test basic conversion

(import (liii check) (liii error) (liii set))


(check-set-mode! 'report-failed)

(define s-to-list (set 1 2 3))

(define l-to-list (set->list s-to-list))
(check (length l-to-list) => 3)
(check-true (set=? (list->set l-to-list) s-to-list))


;; Test type error
(check-catch 'type-error (set->list "not a set"))


(check-report)
