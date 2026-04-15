(import (liii chez) (liii check))


(check-set-mode! 'report-failed)


;; atom? 函数测试
;;
;; 判断给定值是否为原子（非点对类型）。
;;
;; 语法
;; ----
;; (atom? x)
;;
;; 参数
;; ----
;; x : any
;; 任意 Scheme 值。
;;
;; 返回值
;; ----
;; boolean
;; 如果 x 不是点对（pair）则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; atom? 是 Chez Scheme 中的谓词函数，用于区分原子类型和复合类型（点对/列表）。
;; 在 Scheme 中，原子包括：数字、字符串、符号、布尔值、空列表 '()、向量等。
;; 非原子（即点对）是 cons 构造的复合数据结构。
;;
;; 使用场景
;; --------
;; 常用于递归处理树形结构时判断叶子节点，或在宏展开时区分简单值和列表结构。
;;
;; 示例
;; ----
;; (atom? 'symbol) => #t
;; (atom? 123) => #t
;; (atom? "string") => #t
;; (atom? '(a b c)) => #f
;; (atom? '()) => #t


(check-true (atom? 'symbol))
(check-true (atom? 'a))
(check-true (atom? '+))


(check-true (atom? 0))
(check-true (atom? 42))
(check-true (atom? -123))
(check-true (atom? 3.14))
(check-true (atom? 1/2))


(check-true (atom? "hello"))
(check-true (atom? ""))
(check-true (atom? "world"))


(check-true (atom? #t))
(check-true (atom? #f))


(check-true (atom? '()))


(check-false (atom? '(a . b)))
(check-false (atom? '(1 2 3)))
(check-false (atom? '(a b c)))


(check-false (atom? '((a b) (c d))))
(check-false (atom? '(1 (2 3) 4)))


(check-true (atom? #(1 2 3)))
(check-true (atom? #()))


(check-true (atom? atom?))
(check-true (atom? +))


(check-report)
