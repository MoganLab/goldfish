(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

#|
caar
caar 是 Scheme 内置函数，用于获取嵌套序对的第一个元素的第一个分量。该函数是 R7RS 标准的基本列表操作函数之一。此函数等价于 (car (car pair))。

语法
----
(caar pair)

参数
----
pair : pair?
必须是序对或列表。一般为一个包含点对或列表的列表，如 ((a . b) . c) 或 ((a b c) ...)。

返回值
------
任意类型
返回嵌套序对的第一个元素的第一个分量。根据不同序对和嵌套结构的性质，返回类型可以是符号、数字、列表、点对、布尔值等任何对象。

说明
----
1. caar 是 pair? 谓词的重要操作之一，常与 cdr、cdar、cadr 等配合使用处理嵌套序对数据
2. 当应用于列表时，相当于 (car (car list))，需要保证嵌套结构有效且非空
3. 适用于所有嵌套序对数据：包括点对结构 ((a . b) . c) 和嵌套列表 ((a b c) d e f)
4. 当嵌套结构为列表时，caar 返回第一个列表的第一个元素

错误处理
--------
wrong-type-arg
出现以下情况时抛出错误：
1. 参数不是序对或列表类型（如空列表 '()、数字、字符串等）
2. 序对结构的第一个元素本身不是序对或列表（如原子或空列表）
|#

;; 基础测试：显式点对结构
(check (caar '((a . b) . c)) => 'a)
(check (caar '((1 . 2) . 3)) => 1)
(check (caar '((#t . #f) . nil)) => #t)

;; 基础测试：列表结构
(check (caar '((a b c) d e)) => 'a)
(check (caar '((1 2 3) 4 5)) => 1)
(check (caar '((#t #f) x y z)) => #t)

;; 嵌套列表结构
(check (caar '(((a b) c) d e)) => '(a b))
(check (caar '(((() a) b) c)) => '(() a))
(check (caar '(((1 2) 3) 4)) => '(1 2))

;; 混合结构测试
(check (caar '(("hello" . 123) . "world")) => "hello")
(check (caar '((42 . "forty-two") . 99)) => 42)
(check (caar '((#\a . #\b) . nil)) => #\a)

;; 构造器创建的结构
(check (caar (cons (cons 1 2) (cons 3 4))) => 1)
(check (caar (cons (cons 'x 'y) (cons 'z 'w))) => 'x)
(check (caar (cons (list 1 2 3) (list 4 5 6))) => 1)

;; 复杂嵌套构造
(let ((nested (cons (cons (cons 1 2) (cons 3 4)) (cons 5 6))))
  (check (caar nested) => (cons 1 2))
) ;let

;; 涉及空列表的测试
(check-catch 'wrong-type-arg (caar '(() . c)))
(check-catch 'wrong-type-arg (caar '((). ())))

;; 非法参数类型错误
(check-catch 'wrong-type-arg (caar 'a))
(check-catch 'wrong-type-arg (caar 123))
(check-catch 'wrong-type-arg (caar "hello"))
(check-catch 'wrong-type-arg (caar #f))
(check-catch 'wrong-type-arg (caar '()))
(check-catch 'wrong-type-arg (caar '(a b . c)))

;; 返回不同类型测试
(check (caar '(("string" "another") 42)) => "string")
(check (caar '((123 456) 789)) => 123)
(check (caar '(((1 2 3)) 4 5 6)) => (list 1 2 3))
(check (caar '((#f nil "test") x y z)) => #f)

;; edge cases for nested structure
(check (caar '((((a))))) => '((a)))
(check (caar '((((1 2))) 3 4 5)) => '((1 2)))


(check-report)
