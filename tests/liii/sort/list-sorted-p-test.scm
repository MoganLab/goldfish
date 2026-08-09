(import (liii check) (liii sort))


(check-set-mode! 'report-failed)


;; list-sorted?
;; 检查列表是否已按指定比较函数排序。
;;
;; 语法
;; ----
;; (list-sorted? cmp lst)
;;
;; 参数
;; ----
;; cmp : procedure
;; 比较函数，接受两个参数，返回布尔值。
;;
;; lst : list
;; 要检查的列表。
;;
;; 返回值
;; ----
;; boolean
;; 如果列表已排序返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (list-sorted? < '(1 2 3 4 5)) => #t
;; (list-sorted? < '(1 5 1 0 -1)) => #f
;;
;; 错误处理
;; ----
;; cmp 不是过程、lst 不是正规列表时抛出错误


(check-false (list-sorted? < '(1 5 1 0 -1 9 2 4 3)))
(check-true (list-sorted? < '(1 2 3 4 5)))
(check-true (list-sorted? < '()))
(check-true (list-sorted? < '(42)))
(check-true (list-sorted? > '(5 4 3 2 1)))
(check-false (list-sorted? > '(1 2 3 4 5)))

;; 两个元素的边界情况
(check-true (list-sorted? < '(1 2)))
(check-false (list-sorted? < '(2 1)))

;; 相等元素：判定标准是 (not (less-p next prev))，与 SRFI-132 参考实现一致
;; < 允许相邻相等，<= 不允许（1 <= 1 为真即视为逆序）
(check-true (list-sorted? < '(1 1 2 2 3)))
(check-false (list-sorted? <= '(1 1 2 2 3)))
(check-false (list-sorted? > '(1 1 2 2 3)))

;; 自定义比较器（lambda）
(check-true (list-sorted? (lambda (a b) (< (abs a) (abs b))) '(1 -2 3 -4)))
(check-false (list-sorted? (lambda (a b) (< (abs a) (abs b))) '(1 -3 2)))

;; 字符串比较器
(check-true (list-sorted? string<? '("a" "b" "c")))
(check-false (list-sorted? string<? '("b" "a")))

;; 字符比较器
(check-true (list-sorted? char<? '(#\a #\b #\c)))
(check-false (list-sorted? char<? '(#\b #\a)))

;; 错误：lst 不是正规列表
(check-catch 'wrong-type-arg (list-sorted? < '(1 2 . 3)))
(check-catch 'wrong-type-arg (list-sorted? < 3))

;; 错误：cmp 不是过程
(check-catch 'wrong-type-arg (list-sorted? 3 '(1 2 3)))


;; 配合排序函数使用
(check-true (list-sorted? < (list-sort < '(1 5 1 0 -1 9 2 4 3))))
(check-true (list-sorted? < (list-stable-sort < '(1 5 1 0 -1 9 2 4 3))))


(check-report)
