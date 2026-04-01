(import (liii check)
        (scheme base)
) ;import

(check-set-mode! 'report-failed)

#|
null?
判断给定的对象是否为空列表。

语法
----
(null? obj)

参数
----
obj : any
任意类型的对象

返回值
------
boolean?
如果obj是空列表则返回#t，否则返回#f

说明
----
1. 用于检查对象是否为空列表'()
2. 对其他任何类型的对象都返回#f
3. 通常在列表处理中使用，用于判断列表是否为空

特殊规则
---------
- 仅当参数为精确的空列表 '() 时返回 #t
- 所有其他对象，包括向量、字符串、数字等都返回 #f
- 非列表结构也返回 #f（如点对、符号等）

错误处理
---------
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; null? 基本测试：空列表和非空列表
(check (null? '()) => #t)                   ; 空列表
(check (null? '(1)) => #f)                  ; 单元素列表
(check (null? '(a)) => #f)                  ; 单元素符号列表
(check (null? '(a b c)) => #f)              ; 多元素列表
(check (null? '(1 2 3 4 5)) => #f)          ; 长列表

;; null? 特殊结构和边界情况
(check (null? '(())) => #f)                 ; 包含空列表的列表
(check (null? '(() () ())) => #f)           ; 空列表嵌套
(check (null? '((a b) (c d))) => #f)        ; 嵌套列表

;; null? 非列表类型测试 - 全面覆盖
(check (null? #t) => #f)                    ; 布尔值
(check (null? #f) => #f)                    ; 布尔值
(check (null? 0) => #f)                     ; 零
(check (null? 123) => #f)                   ; 整数
(check (null? -456) => #f)                  ; 负整数
(check (null? 3.14) => #f)                  ; 浮点数
(check (null? "") => #f)                   ; 空字符串
(check (null? "hello") => #f)               ; 字符串
(check (null? '#()) => #f)                  ; 空向量
(check (null? '#(1 2 3)) => #f)             ; 向量
(check (null? 'symbol) => #f)               ; 符号
(check (null? '123) => #f)                  ; 数字符号
(check (null? #\a) => #f)                  ; 字符

;; null? 点对结构测试
(check (null? '(a . b)) => #f)              ; 点对不是空列表
(check (null? (cons 1 2)) => #f)            ; cons 创建的点对

;; null? 复杂表达式测试
(check (null? (list)) => #t)                ; 由list创建的空列表
(check (null? (append '() '())) => #t)      ; append结果
(check (null? (cdr '(a))) => #t)            ; cdr结果
(check (null? (cdr '(a b))) => #f)          ; cdr结果

;; null? 与列表操作结合测试
(check (null? (reverse '())) => #t)
(check (null? (reverse '(1))) => #f)

;; null? 错误处理测试
(check-catch 'wrong-number-of-args (null?))
(check-catch 'wrong-number-of-args (null? '() '()))
(check-catch 'wrong-number-of-args (null? 1 2))



(check-report)
