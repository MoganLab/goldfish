(import (liii check) (liii set))


(check-set-mode! 'report-failed)


;; list->set
;; 将列表转换为 set。
;;
;; 语法
;; ----
;; (list->set list)
;;
;; 参数
;; ----
;; list : list
;; 要转换的列表。
;;
;; 返回值
;; ----
;; set
;; 返回包含列表中所有元素的新 set（使用默认比较器，重复元素会被去重）。
;;
;; 注意
;; ----
;; 列表中的重复元素会被去重。
;;
;; 示例
;; ----
;; (list->set '(1 2 3)) => 包含 1, 2, 3 的 set
;; (list->set '(1 2 2 1)) => 包含 1, 2 的 set（重复被去重）
;;
;; 错误处理
;; ----
;; 无异常抛出


(define s-empty (set))
(define comp
  (set-element-comparator s-empty)
) ;define
(define s-1-2-3 (set 1 2 3))
(define s-1-2 (set 1 2))


(define s-list-1 (list->set '(1 2 3)))
(check-true (set? s-list-1))
(check-true (eq? (set-element-comparator s-list-1)
              comp
            ) ;eq?
) ;check-true
(check-true (set=? s-1-2-3 s-list-1))
(check-false (eq? s-1-2-3 s-list-1))


(define s-list-empty (list->set '()))
(check-true (set=? s-empty s-list-empty)
) ;check-true
(check (set-size s-list-empty) => 0)


;; Duplicates in list should be handled
(define s-list-dup
  (list->set '(1 2 2 1))
) ;define
(check-true (set=? s-1-2 s-list-dup))
(check (set-size s-list-dup) => 2)


(check-report)
