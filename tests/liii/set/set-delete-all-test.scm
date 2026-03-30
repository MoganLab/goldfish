(import (liii check)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-delete-all
;; 返回一个新的 set，其中指定列表中的元素被移除。
;;
;; 语法
;; ----
;; (set-delete-all set element-list)
;;
;; 参数
;; ----
;; set : set
;; 初始 set。
;;
;; element-list : list
;; 要移除的元素列表。
;;
;; 返回值
;; ------
;; set
;; 返回一个新的 set。
;;
;; 示例
;; ----
;; (set-delete-all (set 1 2 3) '(1 2)) => 包含 3 的 set

(define s-1-2-3 (set 1 2 3))

;; Test basic delete-all
(define s-del-all (set-delete-all s-1-2-3 '(1 2)))
(check (set-size s-del-all) => 1)
(check-false (set-contains? s-del-all 1))
(check-false (set-contains? s-del-all 2))
(check-true (set-contains? s-del-all 3))

(check-report)
