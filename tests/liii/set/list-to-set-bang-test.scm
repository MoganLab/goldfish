(import (liii check)
  (liii error)
  (liii set)
) ;import


(check-set-mode! 'report-failed)


;; list->set!
;; 将列表元素并入 set（可变操作）。
;;
;; 语法
;; ----
;; (list->set! set list)
;;
;; 参数
;; ----
;; set : set
;; 目标 set。
;;
;; list : list
;; 要并入的元素列表。
;;
;; 返回值
;; ------
;; set
;; 返回修改后的 set（与传入的 set 是同一个对象）。
;;
;; 示例
;; ----
;; (list->set! (set 1 2) '(2 3 4)) => 修改后的 set，包含 1, 2, 3, 4
;;
;; 错误处理
;; ----
;; type-error
;; 当 set 参数不是 set 时抛出。


;; 测试 list->set! 基本行为
(define s-list-merge (set 1 2))
(define s-list-merge-result
  (list->set! s-list-merge '(2 3 4))
) ;define
(check-true (eq? s-list-merge-result s-list-merge)
) ;check-true
(check (set-size s-list-merge) => 4)
(check-true (set-contains? s-list-merge 1)
) ;check-true
(check-true (set-contains? s-list-merge 2)
) ;check-true
(check-true (set-contains? s-list-merge 3)
) ;check-true
(check-true (set-contains? s-list-merge 4)
) ;check-true


;; 测试空列表
(define s-list-empty (set 1 2))
(list->set! s-list-empty '())
(check (set-size s-list-empty) => 2)


;; 测试类型错误
(check-catch 'type-error
  (list->set! "not a set" '(1 2))
) ;check-catch


(check-report)
