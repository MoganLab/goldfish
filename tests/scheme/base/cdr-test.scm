(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

#|
cdr
cdr 是 Scheme 内置函数，用于获取序对的第二个元素（第二个分量）。该函数与 car 配合使用，是 R7RS 标准的基本列表操作函数之一。

语法
----
(cdr pair)

参数
----
pair : pair?
可以是序对（即非空列表或显式点对），不能是空列表或其他对象。

返回值
------
任意类型
返回序对的第二个元素（cdr部分）。根据不同的序对内容，返回类型可以是
列表、符号、数字、点对、布尔值等任何对象。

说明
----
1. cdr 是 pair? 谓词的基本操作之一，与 car 配合使用处理序对数据
2. 当应用于列表时，返回列表除第一个元素外的子列表
3. 适用于所有序对数据：不论是点对 (a . b) 还是非空列表 (a b c ...)

错误处理
--------
wrong-type-arg
当参数不是序对（如空列表 '()、数字、字符串等）时抛出错误。

|#

(check (cdr '(a b c . d)) => '(b c . d))
(check (cdr '(a b c)) => '(b c))

(check (cdr '(1 . 2)) => 2)
(check (cdr '((a b) . c)) => 'c)

(check (cdr (cons 1 2)) => 2)
(check (cdr (cons 'a 'b)) => 'b)

; 错误测试
(check-catch 'wrong-type-arg (cdr '()))
(check-catch 'wrong-type-arg (cdr 123))
(check-catch 'wrong-type-arg (cdr "hello"))
(check-catch 'wrong-type-arg (cdr #t))
(check-catch 'wrong-number-of-args (cdr))
(check-catch 'wrong-number-of-args (cdr '(1 2) '(3 4)))

;; cdr边界条件测试补充
(check (cdr '(a)) => '())                       ; 单元素列表cdr边界
(check (cdr '(1)) => '())                       ; 单元素数字列表cdr边界
(check (cdr '(#t)) => '())                      ; 单元素布尔列表cdr边界
(check (cdr '("hello")) => '())                 ; 单元素字符串列表cdr边界
(check (cdr '(() b c)) => '(b c))               ; 空列表作为首元素的cdr
(check (cdr '((a b))) => '())                   ; 单元素子列表cdr边界
(check (cdr '((((a))))) => '())                 ; 深度嵌套单元素cdr

;; 各种数据类型cdr边界测试
(check (cdr '(123 "text" symbol)) => '("text" symbol))
(check (cdr '(#
ewline #	ab #\space)) => '(#	ab #\space))
(check (cdr '((a b) c d)) => '(c d))
(check (cdr '(#(1 2) #(3 4))) => '(#(3 4)))
(check (cdr '(+ - * /)) => '(- * /))
(check (cdr '('(a b) '(c d))) => '('(c d)))

;; 极端边界条件测试
(check (cdr '((lambda (x) x) (lambda (y) y))) => '((lambda (y) y)))
(check (cdr '((begin 1 2 3) (begin 4 5))) => '((begin 4 5)))

(check (cdr '(a b.c d)) => '(b.c d))
(check (cdr '("中文" "测试")) => '("测试"))


(check-report)
