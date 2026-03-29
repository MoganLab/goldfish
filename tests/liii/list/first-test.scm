(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; first 函数测试
;;
;; 获取列表的第一个元素。
;;
;; 语法
;; ----
;; (first list)
;;
;; 参数
;; ----
;; list : pair?
;; 非空列表或点对结构。
;;
;; 返回值
;; ----
;; any
;; 返回列表的第一个元素(car部分)。
;;
;; 说明
;; ----
;; first函数作为SRFI-1中的选择器函数，相当于(car list)。当应用于非空列表时返回第一个元素，
;; 当应用于点对时返回左侧元素。如果试图在空列表'()上使用first，会抛出错误。
;;
;; 使用场景
;; --------
;; 常与second至tenth等其他选择器函数配合使用，快速访问列表中特定位置的元素。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当应用于空列表时抛出。
;;
;; 示例
;; ----
;; (first '(1 2 3 4 5 6 7 8 9 10)) => 1
;; (first '(left . right)) => 'left
;; (first '(a b c)) => 'a

(check (first '(1 2 3 4 5 6 7 8 9 10)) => 1)
(check (first '(left . right)) => 'left)
(check (first '(a b c)) => 'a)
(check (first '( 42)) => 42)

(check-catch 'wrong-type-arg (first '()))

; 基本功能测试
(check (first '(a)) => 'a)
(check (first '(a b)) => 'a)
(check (first '(a b c d e)) => 'a)

; 点对结构测试
(check (first '(a . b)) => 'a)
(check (first '((1 2) 3 4)) => '(1 2))

; 嵌套结构测试
(check (first '((a b) (c d))) => '(a b))
(check (first '(() a b)) => '())

; 各种数据类型测试
(check (first '((1 2 3) 4 5)) => '(1 2 3))
(check (first '("a" "b" "c")) => "a")
(check (first '(42 43 44)) => 42)
(check (first '(#t #f #t)) => #t)

; 混合类型测试
(check (first '(1 "hello" a)) => 1)
(check (first '(a 1 #t)) => 'a)

(check-report)
