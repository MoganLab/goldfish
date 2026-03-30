(import (liii check)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-delete-all!
;; 从 set 中移除指定列表中的元素（可变操作）。
;;
;; 语法
;; ----
;; (set-delete-all! set element-list)
;;
;; 参数
;; ----
;; set : set
;; 目标 set。
;;
;; element-list : list
;; 要移除的元素列表。
;;
;; 返回值
;; ------
;; set
;; 返回修改后的 set。
;;
;; 示例
;; ----
;; (set-delete-all! (set 1 2 3) '(1 2)) => 修改后的 set，仅包含 3

(define s-1-2-3 (set 1 2 3))

;; Test basic delete-all!
(define s-mut-del-all (set-copy s-1-2-3))
(set-delete-all! s-mut-del-all '(1 2))
(check (set-size s-mut-del-all) => 1)
(check-false (set-contains? s-mut-del-all 1))
(check-false (set-contains? s-mut-del-all 2))
(check-true (set-contains? s-mut-del-all 3))

(check-report)
