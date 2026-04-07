(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector=?
;; 比较两个或多个 flexvector 是否相等。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector=? eq? fv1 fv2 ...)
;;
;; 参数
;; ----
;; eq? : procedure
;;   元素比较函数，(eq? x y) 返回布尔值。
;;
;; fv1, fv2, ... : flexvector
;;   要比较的向量。
;;
;; 返回值
;; -----
;; 如果所有向量长度相等且对应位置的元素都满足 eq?，返回 #t；否则返回 #f。
;; 空参数或单参数返回 #t。
;;
;; 另见
;; ----
;; eq? - 默认相等判断
;; = - 数值比较
;; equal? - 深比较

;; 相等的向量
(check-true (flexvector=? eq? (flexvector 'a 'b) (flexvector 'a 'b)))
(check-true (flexvector=? = (flexvector 1 2 3) (flexvector 1 2 3)))

;; 顺序不同不相等
(check-false (flexvector=? eq? (flexvector 'a 'b) (flexvector 'b 'a)))

;; 长度不同不相等
(check-false (flexvector=? = (flexvector 1 2 3 4 5) (flexvector 1 2 3 4)))

;; 空参数返回 #t
(check-true (flexvector=? eq?))

;; 单参数返回 #t
(check-true (flexvector=? eq? (flexvector 'a)))

;; 多向量比较
(check-true (flexvector=? = (flexvector 1 2) (flexvector 1 2) (flexvector 1 2)))
(check-false (flexvector=? = (flexvector 1 2) (flexvector 1 2) (flexvector 1 3)))

;; 空向量相等
(check-true (flexvector=? eq? (flexvector) (flexvector)))

;; 使用不同比较函数
(check-true (flexvector=? = (flexvector 1 2 3) (flexvector 1 2 3)))
(check-false (flexvector=? = (flexvector 1 2 3) (flexvector 1 2 4)))

;; 字符比较
(check-true (flexvector=? char=? (flexvector #\a #\b) (flexvector #\a #\b)))
(check-false (flexvector=? char=? (flexvector #\a #\b) (flexvector #\A #\B)))

;; 字符串比较（使用 equal?）
(check-true (flexvector=? equal? (flexvector "a" "b") (flexvector "a" "b")))

(check-report)
