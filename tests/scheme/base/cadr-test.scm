(import (liii check)
        (scheme base)
) ;import

(check-set-mode! 'report-failed)

#|
cadr
cadr 是 Scheme 内置函数，用于获取序对的第二个元素的第一个分量。该函数是 R7RS 标准的基本列表操作函数之一，等价于 (car (cdr pair))。

语法
----
(cadr pair)

参数
----
pair : pair?
可以是序对（即非空列表或显式点对），不能是空列表或其他对象。

返回值
------
任意类型
返回序对的第二个元素的第一个分量。根据不同的序对内容，返回类型可以是
符号、数字、列表、点对、布尔值等任何对象。

说明
----
1. cadr 是 pair? 谓词的基本操作之一，与 car、cdr 和 caar 配合使用处理序对数据
2. 当应用于列表时，返回列表的第二个元素
3. 适用于所有序对数据：不论是点对 (a b . c) 还是非空列表 (a b c ...)
4. 等价于 (car (cdr pair))，执行顺序从右到左解析
5. 是 R7RS 标准 car/cdr 组合函数的之一

错误处理
--------
wrong-type-arg
当参数不是序对（如空列表 '()、数字、字符串等）或序对的cadr不是序对时抛出错误。

边界条件
--------
- 当序对长度为1时，cadr 可能不是序对，需要抛出异常
- 单元素列表因没有cadr会引发错误
- 正确支持两元素及以上列表的访问

性能特征
--------
- 时间复杂度：O(1) 恒定时间完成操作
- 空间复杂度：O(1) 不消耗额外栈空间
- 内存分配：直接访问现有序对结构，无新对象创建

数据类型兼容性
-------------
- 支持所有类型的列表和序对结构
- 各元素可以是任意 Scheme 对象
- 正确处理嵌套列表和复杂结构
|#

;; cadr 基础测试 - 各种典型场景
(check (cadr '(a b)) => 'b)
(check (cadr '(a b c)) => 'b)
(check (cadr '(1 2 3 4 5)) => 2)
(check-catch 'wrong-type-arg (cadr '(a . b)))  ; 点对结构，cadr不是pair
(check (cadr '(a b . rest)) => 'b)
(check (cadr '((a . b) c)) => 'c)              ; 有效点对结构

;; cadr 边界测试
(check (cadr '(a b)) => 'b)                     ; 两元素列表边界
(check-catch 'wrong-type-arg (cadr '(only)))    ; 单元素列表异常
(check (cadr '(pair single)) => 'single)        ; 两元素任意值
(check-catch 'wrong-type-arg (cadr '(a . b)))         ; 点对结构

;; 数据类型边界测试
(check (cadr '(42 string symbol #t)) => 'string)           ; 混合类型访问
(check (cadr '("hello" "world" "test")) => "world")      ; 字符串列表
(check (cadr '(#t #f #t)) => #f)                           ; 布尔值列表
(check (cadr '(list vector string)) => 'vector)             ; 类型对象列表
(check (cadr '((a b) (c d) (e f))) => '(c d))              ; 子列表结构

;; 数值边界测试
(check (cadr '(100 200 300 400 500)) => 200)                 ; 整数数值
(check (cadr '(1.1 2.2 3.3 4.4 5.5)) => 2.2)                 ; 浮点数值
(check (cadr '(1/2 2/3 3/4)) => 2/3)                           ; 有理数
(check (cadr '(1+2i 3+4i 5+6i)) => 3+4i)                       ; 复数

;; 任意对象类型测试
(check (cadr '( #a "string" 42 )) => "string")             ; 字符对象
(check (cadr '(if-cond then-block else-block)) => 'then-block) ; Scheme关键字
(check (cadr '((lambda (x) x) (lambda (y) y) (lambda (z) z))) => '(lambda (y) y)) ; lambda过程

;; 构造器创建的结构测试
(check (cadr (list 'a 'b 'c 'd)) => 'b)                       ; list构造器
(check (cadr (cons 'a (cons 'b (cons 'c '())))) => 'b)        ; cons构造器
(check (cadr (append '() '(a b c))) => 'b)                   ; append结果
(check (cadr (reverse '(c b a))) => 'b)                      ; reverse结果

;; Unicode和字符串边界测试
(check (cadr '("中文" "测试" "验证")) => "测试")              ; Unicode字符串
(check (cadr '(#\中 #\文 #\字)) => #\文)               ; Unicode字符

(check-catch 'wrong-type-arg (cadr '()))                      ; 空列表错误
(check-catch 'wrong-type-arg (cadr 123))                      ; 数字错误
(check-catch 'wrong-type-arg (cadr "string"))                 ; 字符串错误
(check-catch 'wrong-type-arg (cadr #t))                       ; 布尔值错误
(check-catch 'wrong-type-arg (cadr #\a))                       ; 字符错误
(check-catch 'wrong-type-arg (cadr #(a b)))                   ; 向量错误

;; 单元素边界异常测试
(check-catch 'wrong-type-arg (cadr '(single)))                ; 单元素列表错误
(check-catch 'wrong-type-arg (cadr '(all)))                   ; 单元素任意值错误

;; 构造器与操作函数链式测试
(check (cadr (list 'car 'cdr 'cons 'append)) => 'cdr)         ; 内部过程对象

;; =======================================
;; [201_12] cadr 补充边界测试和文档完善
;; 根据 201_12.md 要求补充边界值和数据兼容性测试
;; =======================================

;; 边界测试集1：空结构边界
(check-catch 'wrong-type-arg (cadr (cons 'a 'b)))          ; 单点对结构
(check-catch 'wrong-type-arg (cadr (cons 'a '())))         ; 点对与空列表

;; 边界测试集2：极限长度边界  
(check (cadr (make-list 1000 'x)) => 'x)                   ; 极大长度验证
(check (cadr (append '(a) (make-list 999 'x))) => 'x)      ; 长列表性能边界

;; 边界测试集3：特殊对象类型边界
(check (cadr '(#t + #f)) => '+)                             ; 过程对象访问
(check (cadr '(#t #(1 2 3) #f)) => #(1 2 3))                ; 向量作为元素
(check (cadr '(#t #u8(255 128) #f)) => #u8(255 128))        ; 字节向量访问

;; 边界测试集4：Unicode边界测试
(check (cadr '("特殊&符号" "正常字符串")) => "正常字符串")       ; 复杂UTF-8边界

;; 边界测试集5：复合结构异常边界
(check-catch 'wrong-type-arg (cadr (vector 'a 'b)))          ; 向量类型错误
(check-catch 'wrong-number-of-args (cadr))                   ; 零参数错误  
(check-catch 'wrong-number-of-args (cadr '(a b) '(c d)))     ; 多参数错误


(check-report)
