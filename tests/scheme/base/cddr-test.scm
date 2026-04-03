(import (liii check)
        (scheme base)
) ;import

(check-set-mode! 'report-failed)

#|
cddr
cddr 是 Scheme 内置函数，用于获取序对从第三个元素开始的所有分量（移除第1、第2个元素）。该函数是 R7RS 标准的基本列表操作函数之一。此函数等价于 (cdr (cdr pair))。

语法
----
(cddr pair)

参数
----
pair : pair?
可以是序对（即非空列表或显式点对），不能是空列表或其他对象。

返回值
------
任意类型
返回序对第三个元素开始的所有分量。根据不同的序对内容，返回类型可以是
列表、符号、数字、点对、布尔值等任何对象。

说明
----
1. cddr 是 pair? 谓词的基本操作之一.
2. 当应用于列表时，相当于(cdr(cdr list))，需要保证嵌套结构有效且非空
3. 适用于所有序对数据：不论是点对 (a . b) 还是非空列表 (a b c ...)

错误处理
--------
wrong-type-arg
1.当参数不是序对（如空列表 '()、数字、字符串等）时抛出错误。
2.序队结构中的元素数量少于三个元素

|#
(check (cddr '(a b c . d)) => '(c . d))
(check (cddr '(a b c)) => '(c))
(check (cddr '(1 2 . 3)) => 3)
(check (cddr '((a b) c . d)) => 'd)
(check (cddr (cons 'a (cons 'b (cons 'c 'd)))) => '(c . d))

; 错误测试
(check-catch 'wrong-type-arg (cddr '()))
(check-catch 'wrong-type-arg (cddr 123))
(check-catch 'wrong-type-arg (cddr "hello"))
(check-catch 'wrong-type-arg (cddr #t))
(check-catch 'wrong-number-of-args (cddr))
(check-catch 'wrong-number-of-args (cddr '(1 2) '(3 4)))

;; cddr边界条件测试补充
(check (cddr '(a b)) => '())                       ; 单元素列表cddr边界
(check (cddr '(1 2)) => '())                       ; 单元素数字列表cddr边界
(check (cddr '(#t #f)) => '())                      ; 单元素布尔列表cddr边界
(check (cddr '("hello" "world")) => '())                 ; 单元素字符串列表cddr边界
(check (cddr '(() b c)) => '(c))               ; 空列表作为首元素的cddr
;; 各种数据类型cddr边界测试
(check (cddr '(123 "text" symbol)) => '(symbol))
(check (cddr '(#ewline #ab #\space)) => '(#\space))
(check (cddr '((a b) c d)) => '(d))
(check (cddr '(#(1 2) #(3 4) #(5 6))) => '(#(5 6)))
(check (cddr '(+ - * /)) => '(* /))
(check (cddr '('(a b) '(c d) '(e f))) => '('(e f)))

;; 极端边界条件测试
(check (cddr '((lambda (x) x) (lambda (y) y) (lambda (z) z))) => '((lambda (z) z)))
(check (cddr '((begin 1 2 3) (begin 4 5) (begin 6 7))) => '((begin 6 7)))
(check (cddr '(a b c.d)) => '(c.d))
(check (cddr '("中文" "测试" "程序")) => '("程序"))


(check-report)
