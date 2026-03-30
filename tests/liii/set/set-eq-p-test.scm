(import (liii check)
        (liii error)
        (liii set)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;; set=?
;; 检查两个或多个 set 是否相等。
;;
;; 语法
;; ----
;; (set=? set1 set2 ...)
;;
;; 参数
;; ----
;; set1, set2, ... : set
;; 要比较的 set。
;;
;; 返回值
;; ----
;; boolean
;; 如果所有 set 都包含相同的元素，返回 #t；否则返回 #f。
;;
;; 注意
;; ----
;; 比较器必须相同。
;;
;; 示例
;; ----
;; (set=? (set 1) (set 1)) => #t
;; (set=? (set 1) (set 1) (list->set '(1))) => #t
;;
;; 错误处理
;; ----
;; type-error
;; 当任一参数不是 set 时抛出。
;; value-error
;; 当 set 的比较器不同时抛出。

(define s-empty (set))
(define s-1 (set 1))
(define s-1-2 (set 1 2))

(check-true (set=? s-empty s-empty))
(check-true (set=? s-1 s-1))
(check-true (set=? s-1 (set 1)))
(check-false (set=? s-1 s-empty))
(check-false (set=? s-1 s-1-2))
;; Multiple arguments
(check-true (set=? s-1 (set 1) (list->set '(1))))
(check-false (set=? s-1 s-1 s-empty))
(check-catch 'type-error (set=? "not a set" s-1))

;; Test comparator mismatch
(define str-comp (make-comparator string? string=? string<? string-hash))
(define s-str (list->set-with-comparator str-comp '("apple" "banana")))
(check-catch 'value-error (set=? s-1 s-str))

(check-report)
