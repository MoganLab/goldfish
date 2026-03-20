;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (scheme base)
        (liii rich-list)
        (liii lang)
        (liii error)
) ;import

(check-set-mode! 'report-failed)


#|
rich-list@range  
生成一个从起始值到结束值（不包含结束值）的数字序列。

语法
----
(rich-list :range start end . step-and-args)

参数
----
start : integer
序列的起始值（包含）。

end : integer  
序列的结束边界值（不包含）。

step-and-args : list
可选参数，可包含步进值和链式方法参数。

返回值
-----
以rich-list形式返回生成的整数序列。

功能
----
根据起始值、结束值和步进值生成连续整数序列。步进可以是正数或负数，但不能为0。

边界条件
--------
- 步进为0时抛出 value-error 异常
- 步进为正且 start ≥ end：返回空列表
- 步进为负且 start ≤ end：返回空列表

性能特征
--------
- 时间复杂度：O(n)，n为序列长度
- 空间复杂度：O(n)，存储生成的完整序列

兼容性
------
- 支持链式调用
- 所有参数必须为整数
|#

;; 基本测试
(check ((rich-list :range 1 5) :collect) => (list 1 2 3 4))
(check ((rich-list :range 1 5 2) :collect) => (list 1 3))
(check ((rich-list :range 1 6 2) :collect) => (list 1 3 5))
(check ((rich-list :range 5 1 -1) :collect) => (list 5 4 3 2))
(check ((rich-list :range 1 5 :map (lambda (x) (* x 2))) :collect) => (list 2 4 6 8))
(check ((rich-list :range 1 10 1 :map (lambda (x) (+ x 1))) :collect) => (list 2 3 4 5 6 7 8 9 10))
(check ((rich-list :range 5 1 1) :collect) => (list))

;; 边界测试
(check ((rich-list :range 10 1 1) :collect) => (list))
(check ((rich-list :range -5 -1 1) :collect) => (list -5 -4 -3 -2))
(check ((rich-list :range -1 -5 -1) :collect) => (list -1 -2 -3 -4))
(check ((rich-list :range 5 6) :collect) => (list 5))
(check ((rich-list :range 5 6 -1) :collect) => (list))
(check ((rich-list :range 1 5) :length) => 4)
(check ((rich-list :range 1 1) :length) => 0)


#|
rich-list@empty
创建一个空的rich-list对象。

语法
----
(rich-list :empty . args)

参数
----
args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-list形式返回空的列表对象。

说明
----
创建一个不包含任何元素的rich-list。通常用于初始化数据结构或作为
链式操作的起点。

边界条件
--------
- 无参数调用：返回空列表
- 支持链式调用：可与其他rich-list方法组合使用

性能特征
--------
- 时间复杂度：O(1)，固定时间创建
- 空间复杂度：O(1)，创建空对象所需最小内存

兼容性
------
- 与所有rich-list实例方法兼容
- 支持链式调用模式
|#

;; 基本测试
(check ((rich-list :empty) :collect) => ())
(check ((rich-list :empty) :length) => 0)
(check ((rich-list :empty) :empty?) => #t)

;; 边界测试
(check ((rich-list :empty :map (lambda (x) (* x 2))) :collect) => ())
(check ((rich-list :empty :filter (lambda (x) #t)) :collect) => ())
(check ((rich-list :empty :take 0) :collect) => ())
(check ((rich-list :empty :drop 0) :collect) => ())
(check ((rich-list :empty :reverse) :collect) => ())


#|
rich-list@concat
连接两个rich-list为一个新的rich-list。

语法
----
(rich-list :concat lst1 lst2 . args)

参数
----
lst1 : rich-list
第一个待连接的rich-list。

lst2 : rich-list
第二个待连接的rich-list。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-list形式返回连接后的列表对象。

功能
----
将两个rich-list的内容合并为一个新的rich-list，保持原有顺序。
第一个列表的元素在前，第二个列表的元素在后。

边界条件
--------
- 任一为空列表时仍正常连接
- 支持链式调用模式

性能特征
--------
- 时间复杂度：O(n)，n为两个列表长度之和
- 空间复杂度：O(n)，创建新的合并列表

兼容性
------
- 与所有rich-list方法兼容
- 支持链式调用模式
|#

;; 基本测试 - 两个非空列表连接 (保持原来的形式，避免$在还不适的地方)
(check ((rich-list :concat (rich-list '(1 2 3)) (rich-list '(4 5 6))) :collect) => '(1 2 3 4 5 6))
(check ((rich-list :concat (rich-list '(a b)) (rich-list '(c d))) :collect) => '(a b c d))

;; 边界测试 - 第一个列表为空
(check ((rich-list :concat (rich-list :empty) (rich-list '(1 2 3))) :collect) => '(1 2 3))

;; 边界测试 - 第二个列表为空
(check ((rich-list :concat (rich-list '(1 2 3)) (rich-list :empty)) :collect) => '(1 2 3))

;; 边界测试 - 两个列表都为空
(check ((rich-list :concat (rich-list :empty) (rich-list :empty)) :collect) => '())

;; 链式调用测试
(check ((rich-list :concat (rich-list '(1 2)) (rich-list '(3 4))) :map (lambda (x) (* x 2)) :collect) => '(2 4 6 8))
(check ((rich-list :concat (rich-list '(a b)) (rich-list '(c d))) :length) => 4)

;; 验证不改变原列表
(let ((lst1 (rich-list '(1 2 3)))
      (lst2 (rich-list '(4 5 6))))
  (rich-list :concat lst1 lst2)
  (check (lst1 :collect) => '(1 2 3))
  (check (lst2 :collect) => '(4 5 6))
) ;let

#|
rich-list@fill
创建一个指定长度、所有元素都为指定值的rich-list。

语法
----
(rich-list :fill n elem)

参数
----
n : integer
要创建的列表长度，必须为非负整数。

elem : any
列表中要填充的元素值，可以是任意类型的对象。

返回值
-----
以rich-list形式返回包含n个相同元素的新列表。

功能
----
创建一个长度为n的rich-list，其中所有元素的值都设置为elem。
当n为0时返回空列表，当n为负数时抛出value-error异常。

边界条件
--------
- n为负数时：抛出value-error异常，错误信息为"n cannot be negative"
- n为0时：返回空rich-list
- n为整数且n≥0时：正常创建指定长度的rich-list
- elem可以是任意类型的Scheme对象，包括函数、列表、数字等

异常处理
--------
当参数n不是非负整数时，函数会抛出value-error类型异常。
此异常属于(liii error)模块，可在测试环境通过check-catch捕获验证。

性能特征
--------
- 时间复杂度：O(n)，需要逐个创建n个元素
- 空间复杂度：O(n)，需要为n个元素分配内存存储新列表

兼容性
------
- 支持与rich-list所有实例方法链式调用
- 返回的rich-list对象可与现有的实例方法无缝组合
|#
;; 基本测试
(check ((rich-list :fill 5 'x) :collect) => '(x x x x x))
(check ((rich-list :fill 3 10) :collect) => '(10 10 10))
(check ((rich-list :fill 1 "hello") :collect) => '("hello"))
(check ((rich-list :fill 0 'a) :collect) => ())

;; 边界测试 - n为0
(check ((rich-list :fill 0 1) :collect) => ())
(check ((rich-list :fill 0 "test") :collect) => ())

;; 边界测试 - 不同类型的elem
(check ((rich-list :fill 3 #t) :collect) => '(#t #t #t))
(check ((rich-list :fill 2 '(1 2 3)) :collect) => '((1 2 3) (1 2 3)))
(check ((rich-list :fill 4 #f) :collect) => '(#f #f #f #f))

;; 边界测试 - 基础边界
(check ((rich-list :fill 0 'test) :collect) => ())
(check ((rich-list :fill 1 42) :collect) => '(42))
(check ((rich-list :fill 100 0) :length) => 100)
(check ((rich-list :fill 2 '(nested list)) :collect) => '((nested list) (nested list)))

;; 边界测试 - 异常处理
(check-catch 'value-error (rich-list :fill -1 'x))
(check-catch 'value-error (rich-list :fill -3 42))

;; 链式调用测试
(check ((rich-list :fill 4 2) :map (lambda (x) (* x 3)) :collect) => '(6 6 6 6))
(check ((rich-list :fill 3 'a) :length) => 3)
(check ((rich-list :fill 5 1) :filter (lambda (x) (= x 1)) :collect) => '(1 1 1 1 1))
(check ((rich-list :fill 3 100) :take 2 :collect) => '(100 100))


#|
rich-list%collect  
将rich-list转换为标准的Scheme列表。

语法
----
(lst :collect)

参数
----
无

返回值
-----
与rich-list包含相同元素的标准Scheme列表。

功能
----
将rich-list对象中包含的元素数据以标准Scheme列表形式返回。
该函数提供与现有Scheme系统的互操作性，允许用户在使用rich-list的
丰富操作方法后，回到传统列表环境继续处理数据。

边界条件
--------
- 空rich-list返回空列表'()
- 保持原始数据的内部结构和引用完整性

性能特征
--------
- 时间复杂度：O(1)，直接访问内部引用
- 空间复杂度：O(1)，仅返回现有对象引用

兼容性
------
- 适用于任何rich-list实例
- 与标准Scheme环境中的list操作无缝兼容
|#

;; 基本功能测试
(check ($ '(1 2 3 4) :collect) => '(1 2 3 4))
(check ($ '(a b c) :collect) => '(a b c))
(check ($ '() :collect) => '())

;; 边界条件测试 - 空列表
(check ((rich-list :empty) :collect) => '())

;; 边界条件测试 - 单元素列表
(check ((rich-list '(42)) :collect) => '(42))

;; 嵌套结构测试
(check ($ '((1 2) (3 4)) :collect) => '((1 2) (3 4)))
(check ($ '(((a)) ((b))) :collect) => '(((a)) ((b))))

;; 多种类型测试
(check ($ '(#t #f "hello" 42) :collect) => '(#t #f "hello" 42))

;; 链式操作结合测试
(check (($ '(1 2 3 4 5) :filter even?) :collect) => '(2 4))
(check (($ '(1 2 3 4) :map (lambda (x) (* x 2))) :collect) => '(2 4 6 8))
(check (($ '(1 2 3 4 5) :take 3) :collect) => '(1 2 3))

;; 验证返回标准Scheme列表
(let ((result ($ '(a b c) :collect)))
  (check (list? result) => #t)
) ;let

;; 验证列表操作兼容性
(let ((result ($ '(1 2 3) :collect)))
  (check (car result) => 1)
  (check (cadr result) => 2)
  (check (cddr result) => '(3))
) ;let


#|
rich-list%find
在rich-list中查找第一个满足条件的元素。

语法
----
(lst :find pred)

参数
----
pred : procedure
用于测试元素的谓词函数，接受一个参数并返回布尔值。

返回值
-----
以option形式返回找到的第一个满足条件的元素。
- 如果找到匹配元素：返回包含该元素的option对象
- 如果没有找到匹配元素：返回none

功能
----
从列表的开头开始遍历，返回第一个满足谓词条件的元素。
使用option类型包装结果，避免空值异常。

边界条件
--------
- 空列表：返回none
- 没有满足条件的元素：返回none
- 多个满足条件的元素：返回第一个匹配的元素

性能特征
--------
- 时间复杂度：O(n)，最坏情况下需要遍历整个列表
- 空间复杂度：O(1)，仅返回option对象引用

兼容性
------
- 与option类型系统兼容
- 支持链式调用模式
|#

;; 基本测试 - 找到元素
(check (($ '(1 2 3 4 5) :find even?) :get) => 2)
(check (($ '(1 3 5 7 9) :find (lambda (x) (> x 5))) :get) => 7)
(check (($ '(a b c d) :find (lambda (x) (eq? x 'c))) :get) => 'c)

;; 边界测试 - 空列表
(check (($ '() :find (lambda (x) #t)) :defined?) => #f)

;; 边界测试 - 没有匹配元素
(check (($ '(1 3 5 7) :find even?) :defined?) => #f)
(check (($ '(a b c) :find (lambda (x) (eq? x 'z))) :defined?) => #f)

;; 边界测试 - 多个匹配元素，返回第一个
(check (($ '(1 2 4 6 8) :find even?) :get) => 2)
(check (($ '(5 10 15 20) :find (lambda (x) (= (modulo x 5) 0))) :get) => 5)

;; 链式调用测试
(check (($ '(1 2 3 4 5) :filter (lambda (x) (> x 2)) :find even?) :get) => 4)


#|
rich-list%find-last
在rich-list中从后往前查找第一个满足条件的元素。

语法
----
(lst :find-last pred)

参数
----
pred : procedure
用于测试元素的谓词函数，接受一个参数并返回布尔值。

返回值
-----
以option形式返回从后往前找到的第一个满足条件的元素。
- 如果找到匹配元素：返回包含该元素的option对象
- 如果没有找到匹配元素：返回none

功能
----
从列表的末尾开始向前遍历，返回第一个满足谓词条件的元素。
与find方法相反，find-last返回最后一个匹配的元素。
使用option类型包装结果，避免空值异常。

边界条件
--------
- 空列表：返回none
- 没有满足条件的元素：返回none
- 多个满足条件的元素：返回最后一个匹配的元素

性能特征
--------
- 时间复杂度：O(n)，最坏情况下需要遍历整个列表
- 空间复杂度：O(n)，需要反转列表（临时空间）

兼容性
------
- 与option类型系统兼容
- 支持链式调用模式
|#

;; 基本测试 - 找到最后一个匹配元素
(check (($ '(1 2 3 4 5) :find-last even?) :get) => 4)
(check (($ '(1 3 5 7 9) :find-last (lambda (x) (> x 5))) :get) => 9)
(check (($ '(a b c d c) :find-last (lambda (x) (eq? x 'c))) :get) => 'c)

;; 边界测试 - 空列表
(check (($ '() :find-last (lambda (x) #t)) :defined?) => #f)

;; 边界测试 - 没有匹配元素
(check (($ '(1 3 5 7) :find-last even?) :defined?) => #f)
(check (($ '(a b c) :find-last (lambda (x) (eq? x 'z))) :defined?) => #f)

;; 边界测试 - 多个匹配元素，返回最后一个
(check (($ '(1 2 4 6 8) :find-last even?) :get) => 8)
(check (($ '(5 10 15 20) :find-last (lambda (x) (= (modulo x 5) 0))) :get) => 20)

;; 链式调用测试
(check (($ '(1 2 3 4 5) :filter (lambda (x) (> x 1)) :find-last even?) :get) => 4)


#|
rich-list%head
返回rich-list的第一个元素。

语法
----
(lst :head)

参数
----
无

返回值
-----
列表的第一个元素。

功能
----
返回rich-list的第一个元素。如果列表为空，则抛出out-of-range异常。

边界条件
--------
- 空列表：抛出out-of-range异常
- 单元素列表：返回该元素
- 多元素列表：返回第一个元素

异常处理
--------
当列表为空时，抛出out-of-range异常，错误信息为"rich-list%head: list is empty"。

性能特征
--------
- 时间复杂度：O(1)，直接访问列表头元素
- 空间复杂度：O(1)，仅返回元素引用

兼容性
------
- 适用于任何非空rich-list实例
- 与标准Scheme的car操作语义一致
|#

;; 基本测试 - 非空列表
(check ($ '(1 2 3) :head) => 1)
(check ($ '(a b c) :head) => 'a)
(check ($ '(42) :head) => 42)

;; 边界测试 - 单元素列表
(check ($ '(hello) :head) => 'hello)
(check ($ '(#t) :head) => #t)
(check ($ '("test") :head) => "test")

;; 边界测试 - 多元素列表
(check ($ '(1 2 3 4 5) :head) => 1)
(check ($ '(x y z) :head) => 'x)

;; 边界测试 - 异常处理
(check-catch 'out-of-range ($ '() :head))

;; 链式调用测试
(check ($ '(1 2 3) :head) => 1)
(check ($ '(a b c) :take 2 :head) => 'a)


#|
rich-list%head-option
返回rich-list的第一个元素，使用option类型包装结果。

语法
----
(lst :head-option)

参数
----
无

返回值
-----
以option形式返回列表的第一个元素。
- 如果列表不为空：返回包含第一个元素的option对象
- 如果列表为空：返回none

功能
----
安全地获取rich-list的第一个元素，避免空列表异常。
使用option类型包装结果，提供类型安全的空值处理。

边界条件
--------
- 空列表：返回none
- 单元素列表：返回包含该元素的option
- 多元素列表：返回包含第一个元素的option

性能特征
--------
- 时间复杂度：O(1)，直接访问列表头元素
- 空间复杂度：O(1)，仅返回option对象引用

兼容性
------
- 与option类型系统兼容
- 支持链式调用模式
- 适用于任何rich-list实例
|#

;; 基本测试 - 非空列表
(check (($ '(1 2 3) :head-option) :get) => 1)
(check (($ '(a b c) :head-option) :get) => 'a)
(check (($ '(42) :head-option) :get) => 42)

;; 边界测试 - 空列表
(check (($ '() :head-option) :defined?) => #f)

;; 边界测试 - 单元素列表
(check (($ '(hello) :head-option) :get) => 'hello)
(check (($ '(#t) :head-option) :get) => #t)
(check (($ '("test") :head-option) :get) => "test")

;; 边界测试 - 多元素列表
(check (($ '(1 2 3 4 5) :head-option) :get) => 1)
(check (($ '(x y z) :head-option) :get) => 'x)

;; 链式调用测试
(check (($ '(1 2 3) :head-option) :get) => 1)
(check (($ '(a b c) :take 2 :head-option) :get) => 'a)


#|
rich-list%last
返回rich-list的最后一个元素。

语法
----
(lst :last)

参数
----
无

返回值
-----
列表的最后一个元素。

功能
----
返回rich-list的最后一个元素。如果列表为空，则抛出index-error异常。

边界条件
--------
- 空列表：抛出index-error异常
- 单元素列表：返回该元素
- 多元素列表：返回最后一个元素

异常处理
--------
当列表为空时，抛出index-error异常，错误信息为"rich-list%last: empty list"。

性能特征
--------
- 时间复杂度：O(n)，需要反转列表
- 空间复杂度：O(n)，需要临时存储反转后的列表

兼容性
------
- 适用于任何非空rich-list实例
- 与标准Scheme的last操作语义一致
|#

;; 基本测试 - 非空列表
(check ($ '(1 2 3) :last) => 3)
(check ($ '(a b c) :last) => 'c)
(check ($ '(42) :last) => 42)

;; 边界测试 - 单元素列表
(check ($ '(hello) :last) => 'hello)
(check ($ '(#t) :last) => #t)
(check ($ '("test") :last) => "test")

;; 边界测试 - 多元素列表
(check ($ '(1 2 3 4 5) :last) => 5)
(check ($ '(x y z) :last) => 'z)

;; 边界测试 - 异常处理
(check-catch 'index-error ($ '() :last))

;; 链式调用测试
(check ($ '(1 2 3) :last) => 3)
(check ($ '(a b c) :reverse :last) => 'a)


#|
rich-list%last-option
返回rich-list的最后一个元素，使用option类型包装结果。

语法
----
(lst :last-option)

参数
----
无

返回值
-----
以option形式返回列表的最后一个元素。
- 如果列表不为空：返回包含最后一个元素的option对象
- 如果列表为空：返回none

功能
----
安全地获取rich-list的最后一个元素，避免空列表异常。
使用option类型包装结果，提供类型安全的空值处理。

边界条件
--------
- 空列表：返回none
- 单元素列表：返回包含该元素的option
- 多元素列表：返回包含最后一个元素的option

性能特征
--------
- 时间复杂度：O(n)，需要反转列表
- 空间复杂度：O(n)，需要临时存储反转后的列表

兼容性
------
- 与option类型系统兼容
- 支持链式调用模式
- 适用于任何rich-list实例
|#

;; 基本测试 - 非空列表
(check (($ '(1 2 3) :last-option) :get) => 3)
(check (($ '(a b c) :last-option) :get) => 'c)
(check (($ '(42) :last-option) :get) => 42)

;; 边界测试 - 空列表
(check (($ '() :last-option) :defined?) => #f)

;; 边界测试 - 单元素列表
(check (($ '(hello) :last-option) :get) => 'hello)
(check (($ '(#t) :last-option) :get) => #t)
(check (($ '("test") :last-option) :get) => "test")

;; 边界测试 - 多元素列表
(check (($ '(1 2 3 4 5) :last-option) :get) => 5)
(check (($ '(x y z) :last-option) :get) => 'z)

;; 链式调用测试
(check (($ '(1 2 3) :last-option) :get) => 3)
(check (($ '(a b c) :reverse :last-option) :get) => 'a)


#|
rich-list%slice
提取rich-list中指定范围的子列表。

语法
----
(lst :slice from until . args)

参数
----
from : integer
子列表的起始索引（包含）。

until : integer
子列表的结束索引（不包含）。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-list形式返回指定范围的子列表。

功能
----
提取从索引from开始到索引until结束（不包含until）的子列表。
自动处理边界情况，当索引超出范围时自动调整到有效范围。
如果start >= end，返回空列表。

边界条件
--------
- from < 0：自动调整为0
- until > 列表长度：自动调整为列表长度
- from >= until：返回空列表
- 支持链式调用模式

性能特征
--------
- 时间复杂度：O(n)，需要遍历部分列表
- 空间复杂度：O(k)，k为子列表长度

兼容性
------
- 与所有rich-list实例方法兼容
- 支持链式调用模式
|#

;; 基本测试 - 正常范围
(check (($ '(1 2 3 4 5) :slice 1 4) :collect) => '(2 3 4))
(check (($ '(a b c d e) :slice 0 3) :collect) => '(a b c))
(check (($ '(10 20 30 40) :slice 2 4) :collect) => '(30 40))

;; 边界测试 - 空列表
(check (($ '() :slice 0 3) :collect) => ())
(check (($ '() :slice 1 2) :collect) => ())

;; 边界测试 - 单元素列表
(check (($ '(42) :slice 0 1) :collect) => '(42))
(check (($ '(42) :slice 1 2) :collect) => ())

;; 边界测试 - 索引超出范围
(check (($ '(1 2 3) :slice -1 2) :collect) => '(1 2))
(check (($ '(1 2 3) :slice 1 10) :collect) => '(2 3))
(check (($ '(1 2 3) :slice -5 5) :collect) => '(1 2 3))

;; 边界测试 - 无效范围
(check (($ '(1 2 3) :slice 2 1) :collect) => ())
(check (($ '(1 2 3) :slice 3 3) :collect) => ())
(check (($ '(1 2 3) :slice 5 10) :collect) => ())

;; 链式调用测试
(check (($ '(1 2 3 4 5) :slice 1 4 :map (lambda (x) (* x 2))) :collect) => '(4 6 8))
(check (($ '(a b c d e) :slice 1 4 :filter (lambda (x) (not (eq? x 'c)))) :collect) => '(b d))


#|
rich-list%apply
返回rich-list中指定索引位置的元素。

语法
----
(lst :apply n)

参数
----
n : integer
要获取的元素的索引位置，从0开始计数。

返回值
-----
索引位置n处的元素值。

功能
----
返回rich-list中指定索引位置的元素。类似于标准Scheme的list-ref操作，
但使用rich-list的面向对象调用语法。

边界条件
--------
- n为负数时：抛出out-of-range异常
- n大于等于列表长度时：抛出out-of-range异常
- 空列表访问任意索引：抛出wrong-type-arg异常
- n为有效索引时：返回对应位置的元素

异常处理
--------
当索引超出有效范围时，抛出out-of-range异常。
当列表为空时，抛出wrong-type-arg异常。

性能特征
--------
- 时间复杂度：O(n)，需要遍历到指定索引位置
- 空间复杂度：O(1)，仅返回元素引用

兼容性
------
- 适用于任何非空rich-list实例
- 与标准Scheme的list-ref操作语义一致
|#

;; 基本测试 - 正常索引访问
(check ($ '(1 2 3 4 5) :apply 0) => 1)
(check ($ '(a b c d) :apply 1) => 'b)
(check ($ '(10 20 30) :apply 2) => 30)

;; 边界测试 - 第一个元素
(check ($ '(42) :apply 0) => 42)
(check ($ '(hello world) :apply 0) => 'hello)

;; 边界测试 - 最后一个元素
(check ($ '(1 2 3) :apply 2) => 3)
(check ($ '(a b c d e) :apply 4) => 'e)

;; 边界测试 - 中间元素
(check ($ '(1 2 3 4 5) :apply 2) => 3)
(check ($ '(x y z) :apply 1) => 'y)

;; 边界测试 - 异常处理
(check-catch 'out-of-range ($ '(1 2 3) :apply -1))
(check-catch 'out-of-range ($ '(1 2 3) :apply 3))
(check-catch 'wrong-type-arg ($ '() :apply 0))

;; 链式调用测试
(check ($ '(1 2 3 4 5) :filter even? :apply 0) => 2)
(check ($ '(1 2 3 4 5) :map (lambda (x) (* x 2)) :apply 1) => 4)


#|
rich-list%equals
比较两个rich-list是否相等。

语法
----
(lst :equals that)

参数
----
that : rich-list
要比较的另一个rich-list对象。

返回值
-----
布尔值：
- #t：两个rich-list相等
- #f：两个rich-list不相等

功能
----
比较当前rich-list与另一个rich-list是否相等。相等的条件包括：
- 两个列表长度相同
- 对应位置的元素使用class=?函数比较结果为相等

class=?函数支持多种类型的比较：
- 对于case-class实例，调用其:equals方法
- 对于原始数据类型，使用equal?进行比较

边界条件
--------
- 长度不同的列表：返回#f
- 空列表与空列表：返回#t
- 包含不同类型元素的列表：使用class=?进行比较
- 包含嵌套结构的列表：递归比较

性能特征
--------
- 时间复杂度：O(n)，需要遍历两个列表的所有元素
- 空间复杂度：O(1)，仅使用常量额外空间

兼容性
------
- 与所有rich-list实例兼容
- 支持链式调用模式
- 与class=?函数语义一致
|#

;; 基本测试 - 相等列表
(check ($ '(1 2 3) :equals ($ '(1 2 3))) => #t)
(check ($ '(a b c) :equals ($ '(a b c))) => #t)
(check ($ '() :equals ($ '())) => #t)

;; 基本测试 - 不相等列表
(check ($ '(1 2 3) :equals ($ '(1 2 4))) => #f)
(check ($ '(a b c) :equals ($ '(a b d))) => #f)
(check ($ '(1 2 3) :equals ($ '(1 2))) => #f)
(check ($ '(1 2) :equals ($ '(1 2 3))) => #f)

;; 边界测试 - 不同类型元素
(check ($ '(1 2 3) :equals ($ '(1.0 2.0 3.0))) => #f)
(check ($ '(#t #f) :equals ($ '(#t #f))) => #t)
(check ($ '("hello" "world") :equals ($ '("hello" "world"))) => #t)

;; 边界测试 - 嵌套结构
(check ($ '((1 2) (3 4)) :equals ($ '((1 2) (3 4)))) => #t)
(check ($ '((1 2) (3 4)) :equals ($ '((1 2) (3 5)))) => #f)

;; 链式调用测试
(check ($ '(1 2 3) :map (lambda (x) (* x 2)) :equals ($ '(2 4 6))) => #t)
(check ($ '(1 2 3) :filter even? :equals ($ '(2))) => #t)


#|
rich-list%forall
检查rich-list中的所有元素是否都满足给定的谓词条件。

语法
----
(lst :forall pred)

参数
----
pred : procedure
用于测试元素的谓词函数，接受一个参数并返回布尔值。

返回值
-----
布尔值：
- #t：列表中的所有元素都满足谓词条件
- #f：列表中至少有一个元素不满足谓词条件

功能
----
遍历rich-list中的所有元素，检查它们是否都满足给定的谓词条件。
如果列表为空，则返回#t（空列表被认为满足所有条件）。

边界条件
--------
- 空列表：返回#t
- 所有元素满足条件：返回#t
- 至少有一个元素不满足条件：返回#f
- 列表包含不同类型元素：使用谓词函数进行类型安全的比较

性能特征
--------
- 时间复杂度：O(n)，需要遍历整个列表
- 空间复杂度：O(1)，仅使用常量额外空间

兼容性
------
- 与所有rich-list实例兼容
- 支持链式调用模式
- 与标准Scheme的every函数语义一致
|#

;; 基本测试 - 所有元素满足条件
(check ($ '(1 2 3 4 5) :forall (lambda (x) (number? x))) => #t)
(check ($ '(2 4 6 8) :forall even?) => #t)
(check ($ '(#t #t #t) :forall (lambda (x) x)) => #t)

;; 基本测试 - 存在不满足条件的元素
(check ($ '(1 2 3 4 5) :forall even?) => #f)
(check ($ '(1 3 5 7) :forall even?) => #f)
(check ($ '(#t #f #t) :forall (lambda (x) x)) => #f)

;; 边界测试 - 空列表
(check ($ '() :forall (lambda (x) #f)) => #t)
(check ($ '() :forall (lambda (x) #t)) => #t)
(check ($ '() :forall (lambda (x) (number? x))) => #t)

;; 边界测试 - 单元素列表
(check ($ '(42) :forall (lambda (x) (= x 42))) => #t)
(check ($ '(42) :forall (lambda (x) (= x 43))) => #f)
(check ($ '(#t) :forall (lambda (x) x)) => #t)
(check ($ '(#f) :forall (lambda (x) x)) => #f)

;; 边界测试 - 混合类型元素
(check ($ '(1 2 3 "hello") :forall (lambda (x) (number? x))) => #f)
(check ($ '(1 2 3) :forall (lambda (x) (number? x))) => #t)
(check ($ '("a" "b" "c") :forall string?) => #t)

;; 链式调用测试
(check ($ '(1 2 3 4 5) :filter even? :forall even?) => #t)
(check ($ '(1 2 3 4 5) :map (lambda (x) (* x 2)) :forall even?) => #t)
(check ($ '(1 2 3 4 5) :take 3 :forall (lambda (x) (< x 4))) => #t)


#|
rich-list%exists
检查rich-list中是否存在至少一个元素满足给定的谓词条件。

语法
----
(lst :exists pred)

参数
----
pred : procedure
用于测试元素的谓词函数，接受一个参数并返回布尔值。

返回值
-----
布尔值：
- #t：列表中至少有一个元素满足谓词条件
- #f：列表中没有元素满足谓词条件

功能
----
遍历rich-list中的所有元素，检查是否存在至少一个元素满足给定的谓词条件。
如果列表为空，则返回#f（空列表被认为不包含任何满足条件的元素）。

边界条件
--------
- 空列表：返回#f
- 至少有一个元素满足条件：返回#t
- 所有元素都不满足条件：返回#f
- 列表包含不同类型元素：使用谓词函数进行类型安全的比较

性能特征
--------
- 时间复杂度：O(n)，最坏情况下需要遍历整个列表
- 空间复杂度：O(1)，仅使用常量额外空间

兼容性
------
- 与所有rich-list实例兼容
- 支持链式调用模式
- 与标准Scheme的any函数语义一致
|#

;; 基本测试 - 存在满足条件的元素
(check ($ '(1 2 3 4 5) :exists even?) => #t)
(check ($ '(1 3 5 7 9) :exists (lambda (x) (> x 5))) => #t)
(check ($ '(a b c d) :exists (lambda (x) (eq? x 'c))) => #t)

;; 基本测试 - 不存在满足条件的元素
(check ($ '(1 3 5 7) :exists even?) => #f)
(check ($ '(1 2 3 4) :exists (lambda (x) (> x 10))) => #f)
(check ($ '(a b c) :exists (lambda (x) (eq? x 'z))) => #f)

;; 边界测试 - 空列表
(check ($ '() :exists (lambda (x) #t)) => #f)
(check ($ '() :exists (lambda (x) #f)) => #f)
(check ($ '() :exists (lambda (x) (number? x))) => #f)

;; 边界测试 - 单元素列表
(check ($ '(42) :exists (lambda (x) (= x 42))) => #t)
(check ($ '(42) :exists (lambda (x) (= x 43))) => #f)
(check ($ '(#t) :exists (lambda (x) x)) => #t)
(check ($ '(#f) :exists (lambda (x) x)) => #f)

;; 边界测试 - 混合类型元素
(check ($ '(1 2 3 "hello") :exists string?) => #t)
(check ($ '(1 2 3) :exists string?) => #f)
(check ($ '("a" "b" "c") :exists string?) => #t)

;; 链式调用测试
(check ($ '(1 2 3 4 5) :filter even? :exists (lambda (x) (= x 4))) => #t)
(check ($ '(1 2 3 4 5) :map (lambda (x) (* x 2)) :exists (lambda (x) (= x 10))) => #t)
(check ($ '(1 2 3 4 5) :take 3 :exists (lambda (x) (= x 3))) => #t)


#|
rich-list%contains
检查rich-list中是否包含指定的元素。

语法
----
(lst :contains elem)

参数
----
elem : any
要检查是否存在于列表中的元素。

返回值
-----
布尔值：
- #t：列表中包含指定的元素
- #f：列表中不包含指定的元素

功能
----
检查rich-list中是否包含与给定元素相等的元素。
使用equal?函数进行元素比较，支持深层次比较。

边界条件
--------
- 空列表：返回#f
- 包含指定元素：返回#t
- 不包含指定元素：返回#f
- 列表包含嵌套结构：使用equal?进行深层次比较

性能特征
--------
- 时间复杂度：O(n)，最坏情况下需要遍历整个列表
- 空间复杂度：O(1)，仅使用常量额外空间

兼容性
------
- 与所有rich-list实例兼容
- 支持链式调用模式
- 使用equal?函数进行元素比较，与Scheme标准语义一致
|#

;; 基本测试 - 包含指定元素
(check ($ '(1 2 3 4 5) :contains 3) => #t)
(check ($ '(a b c d) :contains 'b) => #t)
(check ($ '("hello" "world") :contains "hello") => #t)

;; 基本测试 - 不包含指定元素
(check ($ '(1 2 3 4 5) :contains 6) => #f)
(check ($ '(a b c) :contains 'z) => #f)
(check ($ '("hello" "world") :contains "test") => #f)

;; 边界测试 - 空列表
(check ($ '() :contains 1) => #f)
(check ($ '() :contains 'a) => #f)
(check ($ '() :contains "test") => #f)

;; 边界测试 - 单元素列表
(check ($ '(42) :contains 42) => #t)
(check ($ '(42) :contains 43) => #f)
(check ($ '(#t) :contains #t) => #t)
(check ($ '(#f) :contains #f) => #t)

;; 边界测试 - 嵌套结构比较
(check ($ '((1 2) (3 4)) :contains '(1 2)) => #t)
(check ($ '((1 2) (3 4)) :contains '(1 3)) => #f)
(check ($ '(#(1 2) #(3 4)) :contains #(1 2)) => #t)

;; 链式调用测试
(check ($ '(1 2 3 4 5) :filter even? :contains 4) => #t)
(check ($ '(1 2 3 4 5) :map (lambda (x) (* x 2)) :contains 10) => #t)
(check ($ '(1 2 3 4 5) :take 3 :contains 3) => #t)


#|
rich-list%flat-map
对rich-list中的每个元素应用映射函数，然后将所有结果列表连接为一个新的rich-list。

语法
----
(lst :flat-map f . args)

参数
----
f : procedure
映射函数，接受一个参数并返回一个列表。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-list形式返回连接后的列表对象。

功能
----
将映射函数f应用到rich-list的每个元素上，然后将所有结果列表连接起来。
相当于先执行map操作，然后执行flatten操作。

边界条件
--------
- 空列表：返回空列表
- 映射函数返回空列表时：该元素不贡献任何结果
- 映射函数返回多个元素的列表时：所有元素都会被包含在结果中
- 支持链式调用模式

性能特征
--------
- 时间复杂度：O(n × m)，其中n是列表长度，m是映射函数返回列表的平均长度
- 空间复杂度：O(n × m)，需要存储所有中间结果

兼容性
------
- 与所有rich-list实例方法兼容
- 支持链式调用模式
- 与标准Scheme的append-map函数语义一致
|#

;; 基本测试 - 简单映射和连接
(check (($ '(1 2 3) :flat-map (lambda (x) (list x x))) :collect) => '(1 1 2 2 3 3))
(check (($ '(a b) :flat-map (lambda (x) (list x (symbol->string x)))) :collect) => '(a "a" b "b"))

;; 边界测试 - 空列表
(check (($ '() :flat-map (lambda (x) (list x x))) :collect) => ())

;; 边界测试 - 映射函数返回空列表
(check (($ '(1 2 3) :flat-map (lambda (x) '())) :collect) => ())

;; 边界测试 - 映射函数返回单元素列表
(check (($ '(1 2 3) :flat-map (lambda (x) (list x))) :collect) => '(1 2 3))

;; 边界测试 - 映射函数返回多元素列表
(check (($ '(1 2) :flat-map (lambda (x) (list x (* x 2) (* x 3)))) :collect) => '(1 2 3 2 4 6))

;; 链式调用测试
(check (($ '(1 2 3) :flat-map (lambda (x) (list x x)) :map (lambda (x) (* x 2))) :collect) => '(2 2 4 4 6 6))
(check (($ '(1 2 3) :filter even? :flat-map (lambda (x) (list x x))) :collect) => '(2 2))

;; 嵌套结构测试
(check (($ '((1 2) (3 4)) :flat-map (lambda (x) x)) :collect) => '(1 2 3 4))
(check (($ '((a) (b c) (d e f)) :flat-map (lambda (x) x)) :collect) => '(a b c d e f))

;; 复杂映射函数测试
(check (($ '(1 2 3) :flat-map (lambda (x) (if (even? x) (list x (* x 2)) (list x)))) :collect) => '(1 2 4 3))


#|
rich-list%for-each
对rich-list中的每个元素应用指定的函数（主要用于副作用操作）。

语法
----
(lst :for-each f)

参数
----
f : procedure
要应用到每个元素的函数，接受一个参数，返回值被忽略。

返回值
-----
无返回值（返回未定义的值）。

功能
----
遍历rich-list中的所有元素，对每个元素应用函数f。
主要用于执行副作用操作，如打印、修改外部状态等。
与map方法不同，for-each不收集结果，也不返回新的rich-list。

边界条件
--------
- 空列表：不执行任何操作
- 函数f可以执行任意副作用操作
- 不保证函数f的执行顺序（但通常按列表顺序执行）
- 支持各种类型的元素和函数

性能特征
--------
- 时间复杂度：O(n)，需要遍历整个列表
- 空间复杂度：O(1)，不创建新的数据结构

兼容性
------
- 与所有rich-list实例兼容
- 支持链式调用模式
- 与标准Scheme的for-each函数语义一致
|#

;; 基本测试 - 副作用操作（使用累加器验证函数执行）
(let ((counter 0))
  ((rich-list '(1 2 3)) :for-each (lambda (x) (set! counter (+ counter x))))
  (check counter => 6)
) ;let

(let ((result '()))
  ((rich-list '(a b c)) :for-each (lambda (x) (set! result (cons x result))))
  (check result => '(c b a))
) ;let

;; 边界测试 - 空列表
(let ((counter 0))
  ((rich-list '()) :for-each (lambda (x) (set! counter (+ counter 1))))
  (check counter => 0)
) ;let

;; 边界测试 - 单元素列表
(let ((result '()))
  ((rich-list '(42)) :for-each (lambda (x) (set! result (cons x result))))
  (check result => '(42))
) ;let

;; 边界测试 - 不同类型的元素
(let ((numbers '())
      (strings '())
      (symbols '()))
  ((rich-list '(1 "hello" world)) :for-each
      (lambda (x)
        (cond
          ((number? x) (set! numbers (cons x numbers)))
          ((string? x) (set! strings (cons x strings)))
          ((symbol? x) (set! symbols (cons x symbols)))
        ) ;cond
      ) ;lambda
  ) ;
  (check numbers => '(1))
  (check strings => '("hello"))
  (check symbols => '(world))
) ;let

;; 链式调用测试 - 在链式操作中使用for-each
(let ((sum 0))
  (let ((filtered-list ((rich-list '(1 2 3 4 5)) :filter even?)))
    (filtered-list :for-each (lambda (x) (set! sum (+ sum x))))
    (check sum => 6)
  ) ;let
) ;let

(let ((result '()))
  (let ((mapped-list ((rich-list '(1 2 3 4 5)) :map (lambda (x) (* x 2)))))
    (mapped-list :for-each (lambda (x) (set! result (cons x result))))
    (check result => '(10 8 6 4 2))
  ) ;let
) ;let

;; 验证for-each不返回值（主要测试副作用，不检查返回值）
;; 注意：for-each主要用于副作用操作，返回值通常未定义


#|
rich-list%take-right
从rich-list的末尾开始提取指定数量的元素。

语法
----
(lst :take-right n . args)

参数
----
n : integer
要从列表末尾提取的元素数量，必须为非负整数。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-list形式返回从末尾开始的n个元素。

功能
----
从rich-list的末尾开始提取指定数量的元素，保持原有顺序。
与take方法方向相反，take-right从列表末尾开始取元素。

边界条件
--------
- n为负数时：返回空列表
- n为0时：返回空列表
- n大于等于列表长度时：返回整个列表
- n小于列表长度时：返回末尾的n个元素

异常处理
--------
当参数n不是整数时，抛出type-error异常。
当参数data不是列表时，抛出type-error异常。

性能特征
--------
- 时间复杂度：O(n)，需要计算列表长度
- 空间复杂度：O(k)，k为提取的元素数量

兼容性
------
- 与所有rich-list实例方法兼容
- 支持链式调用模式
- 与标准Scheme的take-right函数语义一致
|#

;; 基本测试 - 正常提取末尾元素
(check (($ '(1 2 3 4 5) :take-right 3) :collect) => '(3 4 5))
(check (($ '(a b c d e) :take-right 2) :collect) => '(d e))
(check (($ '(10 20 30 40) :take-right 1) :collect) => '(40))

;; 基本测试 - 提取整个列表
(check (($ '(1 2 3) :take-right 3) :collect) => '(1 2 3))
(check (($ '(a b c) :take-right 5) :collect) => '(a b c))

;; 边界测试 - 空列表
(check (($ '() :take-right 3) :collect) => ())
(check (($ '() :take-right 0) :collect) => ())

;; 边界测试 - 单元素列表
(check (($ '(42) :take-right 1) :collect) => '(42))
(check (($ '(42) :take-right 0) :collect) => ())
(check (($ '(42) :take-right 2) :collect) => '(42))

;; 边界测试 - 提取0个元素
(check (($ '(1 2 3 4 5) :take-right 0) :collect) => ())
(check (($ '(a b c) :take-right 0) :collect) => ())

;; 边界测试 - 提取负数个元素
(check (($ '(1 2 3) :take-right -1) :collect) => ())
(check (($ '(a b c) :take-right -5) :collect) => ())

;; 边界测试 - 提取比列表长度多的元素
(check (($ '(1 2 3) :take-right 10) :collect) => '(1 2 3))
(check (($ '(a b) :take-right 5) :collect) => '(a b))

;; 链式调用测试
(check (($ '(1 2 3 4 5) :take-right 3 :map (lambda (x) (* x 2))) :collect) => '(6 8 10))
(check (($ '(a b c d e) :take-right 4 :filter (lambda (x) (not (eq? x 'c)))) :collect) => '(b d e))
(check ($ '(1 2 3 4 5) :take-right 2 :collect) => '(4 5))
(check ($ '(1 2 3 4 5) :take-right 2 :length) => 2)

;; 与take方法对比测试
(check (($ '(1 2 3 4 5) :take 3) :collect) => '(1 2 3))
(check (($ '(1 2 3 4 5) :take-right 3) :collect) => '(3 4 5))

;; 验证不改变原列表
(let ((lst (rich-list '(1 2 3 4 5))))
  (lst :take-right 2)
  (check (lst :collect) => '(1 2 3 4 5))
) ;let


#|
rich-list%drop-right
从rich-list的末尾开始删除指定数量的元素。

语法
----
(lst :drop-right n . args)

参数
----
n : integer
要从列表末尾删除的元素数量，必须为整数。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-list形式返回删除末尾n个元素后的列表。

功能
----
从rich-list的末尾开始删除指定数量的元素，返回剩余的前面部分。
与drop方法方向相反，drop-right从列表末尾开始删除元素。

边界条件
--------
- n为负数时：返回整个列表（不删除任何元素）
- n为0时：返回整个列表
- n大于等于列表长度时：返回空列表
- n小于列表长度时：返回删除末尾n个元素后的列表

异常处理
--------
当参数n不是整数时，抛出type-error异常。
当参数data不是列表时，抛出type-error异常。

性能特征
--------
- 时间复杂度：O(n)，需要计算列表长度
- 空间复杂度：O(k)，k为剩余的元素数量

兼容性
------
- 与所有rich-list实例方法兼容
- 支持链式调用模式
- 与标准Scheme的drop-right函数语义一致
|#

;; 基本测试 - 正常删除末尾元素
(check (($ '(1 2 3 4 5) :drop-right 3) :collect) => '(1 2))
(check (($ '(a b c d e) :drop-right 2) :collect) => '(a b c))
(check (($ '(10 20 30 40) :drop-right 1) :collect) => '(10 20 30))

;; 基本测试 - 删除整个列表
(check (($ '(1 2 3) :drop-right 3) :collect) => ())
(check (($ '(a b c) :drop-right 5) :collect) => ())

;; 边界测试 - 空列表
(check (($ '() :drop-right 3) :collect) => ())
(check (($ '() :drop-right 0) :collect) => ())

;; 边界测试 - 单元素列表
(check (($ '(42) :drop-right 1) :collect) => ())
(check (($ '(42) :drop-right 0) :collect) => '(42))
(check (($ '(42) :drop-right 2) :collect) => ())

;; 边界测试 - 删除0个元素
(check (($ '(1 2 3 4 5) :drop-right 0) :collect) => '(1 2 3 4 5))
(check (($ '(a b c) :drop-right 0) :collect) => '(a b c))

;; 边界测试 - 删除负数个元素
(check (($ '(1 2 3) :drop-right -1) :collect) => '(1 2 3))
(check (($ '(a b c) :drop-right -5) :collect) => '(a b c))

;; 边界测试 - 删除比列表长度多的元素
(check (($ '(1 2 3) :drop-right 10) :collect) => ())
(check (($ '(a b) :drop-right 5) :collect) => ())

;; 链式调用测试
(check (($ '(1 2 3 4 5) :drop-right 3 :map (lambda (x) (* x 2))) :collect) => '(2 4))
(check (($ '(a b c d e) :drop-right 4 :filter (lambda (x) (not (eq? x 'a)))) :collect) => ())
(check ($ '(1 2 3 4 5) :drop-right 2 :collect) => '(1 2 3))
(check ($ '(1 2 3 4 5) :drop-right 2 :length) => 3)

;; 与drop方法对比测试
(check (($ '(1 2 3 4 5) :drop 3) :collect) => '(4 5))
(check (($ '(1 2 3 4 5) :drop-right 3) :collect) => '(1 2))

;; 验证不改变原列表
(let ((lst (rich-list '(1 2 3 4 5))))
  (lst :drop-right 2)
  (check (lst :collect) => '(1 2 3 4 5))
) ;let


#|
rich-list%count
统计rich-list中满足条件的元素数量或返回列表长度。

语法
----
(lst :count)
(lst :count pred)

参数
----
无参数：返回列表长度
pred : procedure (可选)
用于测试元素的谓词函数，接受一个参数并返回布尔值。

返回值
-----
整数：
- 无参数调用：返回列表长度
- 有谓词函数调用：返回满足谓词条件的元素数量

功能
----
提供两种统计功能：
1. 无参数调用：返回rich-list的长度，与%length方法功能相同
2. 有谓词函数调用：统计列表中满足给定条件的元素数量

边界条件
--------
- 空列表：无参数调用返回0，有谓词调用返回0
- 所有元素满足条件：返回列表长度
- 没有元素满足条件：返回0
- 部分元素满足条件：返回满足条件的元素数量
- 参数数量错误（多于1个）：抛出wrong-number-of-args异常

性能特征
--------
- 时间复杂度：
  - 无参数调用：O(1)，直接返回预计算的列表长度
  - 有谓词调用：O(n)，需要遍历整个列表
- 空间复杂度：O(1)，仅使用常量额外空间

兼容性
------
- 与所有rich-list实例兼容
- 支持链式调用模式
- 与标准Scheme的count函数语义一致
|#

;; 基本测试 - 无参数调用（返回列表长度）
(check ($ '(1 2 3 4 5) :count) => 5)
(check ($ '(a b c) :count) => 3)
(check ($ '() :count) => 0)
(check ($ '(42) :count) => 1)

;; 基本测试 - 有谓词函数调用（统计满足条件的元素）
(check ($ '(1 2 3 4 5) :count even?) => 2)
(check ($ '(1 3 5 7) :count even?) => 0)
(check ($ '(2 4 6 8) :count even?) => 4)
(check ($ '(a b c d) :count (lambda (x) (eq? x 'b))) => 1)

;; 边界测试 - 空列表
(check ($ '() :count) => 0)
(check ($ '() :count (lambda (x) #t)) => 0)
(check ($ '() :count (lambda (x) #f)) => 0)

;; 边界测试 - 单元素列表
(check ($ '(42) :count) => 1)
(check ($ '(42) :count (lambda (x) (= x 42))) => 1)
(check ($ '(42) :count (lambda (x) (= x 43))) => 0)
(check ($ '(#t) :count (lambda (x) x)) => 1)
(check ($ '(#f) :count (lambda (x) x)) => 0)

;; 边界测试 - 所有元素满足条件
(check ($ '(2 4 6 8) :count even?) => 4)
(check ($ '(#t #t #t) :count (lambda (x) x)) => 3)
(check ($ '(1 2 3) :count (lambda (x) (number? x))) => 3)

;; 边界测试 - 没有元素满足条件
(check ($ '(1 3 5 7) :count even?) => 0)
(check ($ '(#f #f #f) :count (lambda (x) x)) => 0)
(check ($ '(1 2 3) :count string?) => 0)

;; 边界测试 - 部分元素满足条件
(check ($ '(1 2 3 4 5) :count even?) => 2)
(check ($ '(#t #f #t) :count (lambda (x) x)) => 2)
(check ($ '(1 "hello" 2 "world") :count number?) => 2)

;; 链式调用测试
(check ($ '(1 2 3 4 5) :filter even? :count) => 2)
(check ($ '(1 2 3 4 5) :map (lambda (x) (* x 2)) :count even?) => 5)
(check ($ '(1 2 3 4 5) :take 3 :count even?) => 1)

;; 验证不改变原列表
(let ((lst (rich-list '(1 2 3 4 5))))
  (lst :count)
  (check (lst :collect) => '(1 2 3 4 5))
  (lst :count even?)
  (check (lst :collect) => '(1 2 3 4 5))
) ;let

;; 与%length方法对比测试
(check ($ '(1 2 3 4 5) :count) => ($ '(1 2 3 4 5) :length))
(check ($ '() :count) => ($ '() :length))
(check ($ '(42) :count) => ($ '(42) :length))


#|
rich-list%fold-right
对rich-list中的元素从左到右进行折叠操作。

语法
----
(lst :fold-right initial f)

参数
----
initial : any
折叠操作的初始值。

f : procedure
折叠函数，接受两个参数 (current-element accumulator) 并返回新的累加值。

返回值
-----
折叠操作的结果值。

功能
----
从左到右遍历rich-list中的所有元素，使用折叠函数f将每个元素与当前的累加值组合。
折叠过程：f(elem_1, initial) -> result_1, f(elem_2, result_1) -> result_2, ...

边界条件
--------
- 空列表：返回初始值initial
- 单元素列表：返回f(第一个元素, initial)
- 多元素列表：从左到右依次应用折叠函数
- 支持各种类型的初始值和折叠函数

性能特征
--------
- 时间复杂度：O(n)，需要遍历整个列表
- 空间复杂度：O(1)，仅使用常量额外空间

兼容性
------
- 与所有rich-list实例兼容
- 支持链式调用模式
- 与Goldfish Scheme的fold-right函数语义一致
|#

;; 基本测试 - 数值求和
(check ($ '(1 2 3 4 5) :fold-right 0 +) => 15)
(check ($ '(10 20 30) :fold-right 0 +) => 60)

;; 基本测试 - 数值求积
(check ($ '(1 2 3 4) :fold-right 1 *) => 24)
(check ($ '(2 3 4) :fold-right 1 *) => 24)

;; 基本测试 - 字符串连接
(check ($ '("a" "b" "c") :fold-right "" string-append) => "abc")
(check ($ '("hello" " " "world") :fold-right "" string-append) => "hello world")

;; 边界测试 - 空列表
(check ($ '() :fold-right 0 +) => 0)
(check ($ '() :fold-right 1 *) => 1)
(check ($ '() :fold-right "" string-append) => "")

;; 边界测试 - 单元素列表
(check ($ '(42) :fold-right 0 +) => 42)
(check ($ '(5) :fold-right 1 *) => 5)
(check ($ '("test") :fold-right "" string-append) => "test")

;; 边界测试 - 复杂折叠函数
(check ($ '(1 2 3 4) :fold-right '() (lambda (x acc) (cons x acc))) => '(1 2 3 4))
(check ($ '(1 2 3 4) :fold-right 0 (lambda (x acc) (+ (* x x) acc))) => 30)

;; 链式调用测试
(check (($ '(1 2 3 4 5) :filter even?) :fold-right 0 +) => 6)
(check (($ '(1 2 3 4 5) :map (lambda (x) (* x 2))) :fold-right 0 +) => 30)
(check (($ '(1 2 3 4 5) :take 3) :fold-right 1 *) => 6)

;; 验证不改变原列表
(let ((lst (rich-list '(1 2 3 4 5))))
  (lst :fold-right 0 +)
  (check (lst :collect) => '(1 2 3 4 5))
) ;let


#|
rich-list%fold
对rich-list中的元素从右到左进行折叠操作。

语法
----
(lst :fold initial f)

参数
----
initial : any
折叠操作的初始值。

f : procedure
折叠函数，接受两个参数 (current-element accumulator) 并返回新的累加值。

返回值
-----
折叠操作的结果值。

功能
----
从右到左遍历rich-list中的所有元素，使用折叠函数f将每个元素与当前的累加值组合。
折叠过程：f(elem_n, initial) -> result_n, f(elem_{n-1}, result_n) -> result_{n-1}, ...

边界条件
--------
- 空列表：返回初始值initial
- 单元素列表：返回f(第一个元素, initial)
- 多元素列表：从右到左依次应用折叠函数
- 支持各种类型的初始值和折叠函数

性能特征
--------
- 时间复杂度：O(n)，需要遍历整个列表
- 空间复杂度：O(1)，仅使用常量额外空间

兼容性
------
- 与所有rich-list实例兼容
- 支持链式调用模式
- 与Goldfish Scheme的fold函数语义一致
|#

;; 基本测试 - 数值求和
(check ($ '(1 2 3 4 5) :fold 0 +) => 15)
(check ($ '(10 20 30) :fold 0 +) => 60)

;; 基本测试 - 数值求积
(check ($ '(1 2 3 4) :fold 1 *) => 24)
(check ($ '(2 3 4) :fold 1 *) => 24)

;; 基本测试 - 字符串连接
(check ($ '("a" "b" "c") :fold "" string-append) => "cba")
(check ($ '("hello" " " "world") :fold "" string-append) => "world hello")

;; 边界测试 - 空列表
(check ($ '() :fold 0 +) => 0)
(check ($ '() :fold 1 *) => 1)
(check ($ '() :fold "" string-append) => "")

;; 边界测试 - 单元素列表
(check ($ '(42) :fold 0 +) => 42)
(check ($ '(5) :fold 1 *) => 5)
(check ($ '("test") :fold "" string-append) => "test")

;; 边界测试 - 复杂折叠函数
(check ($ '(1 2 3 4) :fold '() (lambda (x acc) (cons x acc))) => '(4 3 2 1))
(check ($ '(1 2 3 4) :fold 0 (lambda (x acc) (+ (* x x) acc))) => 30)

;; 链式调用测试
(check (($ '(1 2 3 4 5) :filter even?) :fold 0 +) => 6)
(check (($ '(1 2 3 4 5) :map (lambda (x) (* x 2))) :fold 0 +) => 30)
(check (($ '(1 2 3 4 5) :take 3) :fold 1 *) => 6)

;; 验证不改变原列表
(let ((lst (rich-list '(1 2 3 4 5))))
  (lst :fold 0 +)
  (check (lst :collect) => '(1 2 3 4 5))
) ;let

;; fold和fold-right对比测试
(check ($ '(1 2 3) :fold '() (lambda (x acc) (cons x acc))) => '(3 2 1))
(check ($ '(1 2 3) :fold-right '() (lambda (x acc) (cons x acc))) => '(1 2 3))


#|
rich-list%sort-with
使用自定义比较函数对rich-list进行稳定排序。

语法
----
(lst :sort-with less-p . args)

参数
----
less-p : procedure
比较函数，接受两个参数 (x y)，当 x < y 时返回 #t。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-list形式返回排序后的列表。

功能
----
使用自定义比较函数对rich-list中的元素进行稳定排序。
稳定排序保证相等元素的相对顺序保持不变。
支持链式调用模式。

边界条件
--------
- 空列表：返回空列表
- 单元素列表：返回原列表（无需排序）
- 已排序列表：返回原列表
- 逆序列表：返回升序排列的列表
- 包含重复元素的列表：保持重复元素的相对顺序

性能特征
--------
- 时间复杂度：O(n log n)，使用稳定排序算法
- 空间复杂度：O(n)，需要存储排序后的列表

兼容性
------
- 与所有rich-list实例方法兼容
- 支持链式调用模式
- 使用稳定排序算法，与标准Scheme的排序语义一致
|#

;; 基本测试 - 数值升序排序
(check (($ '(5 2 8 1 9) :sort-with <) :collect) => '(1 2 5 8 9))
(check (($ '(10 3 7 2 6) :sort-with <) :collect) => '(2 3 6 7 10))

;; 基本测试 - 数值降序排序
(check (($ '(5 2 8 1 9) :sort-with >) :collect) => '(9 8 5 2 1))
(check (($ '(10 3 7 2 6) :sort-with >) :collect) => '(10 7 6 3 2))

;; 基本测试 - 字符串排序
(check (($ '("banana" "apple" "cherry") :sort-with string<?) :collect) => '("apple" "banana" "cherry"))
(check (($ '("zebra" "ant" "cat") :sort-with string>?) :collect) => '("zebra" "cat" "ant"))

;; 边界测试 - 空列表
(check (($ '() :sort-with <) :collect) => ())

;; 边界测试 - 单元素列表
(check (($ '(42) :sort-with <) :collect) => '(42))
(check (($ '("hello") :sort-with string<?) :collect) => '("hello"))

;; 边界测试 - 已排序列表
(check (($ '(1 2 3 4 5) :sort-with <) :collect) => '(1 2 3 4 5))
(check (($ '(5 4 3 2 1) :sort-with >) :collect) => '(5 4 3 2 1))

;; 边界测试 - 稳定排序（保持相等元素的相对顺序）
(check (($ '((1 . "a") (2 . "b") (1 . "c") (3 . "d"))
          :sort-with (lambda (x y) (< (car x) (car y))))
        :collect) => '((1 . "a") (1 . "c") (2 . "b") (3 . "d")))

;; 边界测试 - 复杂比较函数
(check (($ '(10 2 25 7 100) :sort-with (lambda (x y) (< (modulo x 10) (modulo y 10))))
        :collect) => '(10 100 2 25 7))

;; 链式调用测试
(check (($ '(5 2 8 1 9) :sort-with < :map (lambda (x) (* x 2))) :collect) => '(2 4 10 16 18))
(check (($ '(10 3 7 2 6) :sort-with > :filter even?) :collect) => '(10 6 2))
(check (($ '(5 2 8 1 9) :sort-with < :take 3) :collect) => '(1 2 5))

;; 验证不改变原列表
(let ((lst (rich-list '(5 2 8 1 9))))
  (lst :sort-with <)
  (check (lst :collect) => '(5 2 8 1 9))
) ;let


#|
rich-list%sort-by
使用键提取函数对rich-list进行稳定排序。

语法
----
(lst :sort-by f . args)

参数
----
f : procedure
键提取函数，接受一个元素并返回用于比较的键值。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-list形式返回排序后的列表。

功能
----
对rich-list中的元素进行稳定排序，使用键提取函数f为每个元素生成比较键。
使用 < 操作符对键值进行比较，因此键值必须是可比较的类型（通常是数字）。
稳定排序保证相等键值元素的相对顺序保持不变。
支持链式调用模式。

边界条件
--------
- 空列表：返回空列表
- 单元素列表：返回原列表（无需排序）
- 已排序列表：返回原列表
- 逆序列表：返回升序排列的列表
- 包含重复键值的元素：保持重复键值元素的相对顺序

性能特征
--------
- 时间复杂度：O(n log n)，使用稳定排序算法
- 空间复杂度：O(n)，需要存储排序后的列表

兼容性
------
- 与所有rich-list实例方法兼容
- 支持链式调用模式
- 使用稳定排序算法，与标准Scheme的排序语义一致
|#

;; 基本测试 - 按绝对值排序
(check (($ '(-5 2 -8 1 9) :sort-by abs) :collect) => '(1 2 -5 -8 9))
(check (($ '(-10 3 -7 2 6) :sort-by abs) :collect) => '(2 3 6 -7 -10))

;; 基本测试 - 按字符串长度排序
(check (($ '("banana" "apple" "cherry") :sort-by string-length) :collect) => '("apple" "banana" "cherry"))
(check (($ '("zebra" "ant" "cat") :sort-by string-length) :collect) => '("ant" "cat" "zebra"))

;; 基本测试 - 按结构体字段排序
(check (($ '((1 . "a") (3 . "b") (2 . "c")) :sort-by car) :collect) => '((1 . "a") (2 . "c") (3 . "b")))
(check (($ '(("apple" . 5) ("banana" . 2) ("cherry" . 8)) :sort-by cdr) :collect) => '(("banana" . 2) ("apple" . 5) ("cherry" . 8)))

;; 边界测试 - 空列表
(check (($ '() :sort-by abs) :collect) => ())

;; 边界测试 - 单元素列表
(check (($ '(42) :sort-by abs) :collect) => '(42))
(check (($ '("hello") :sort-by string-length) :collect) => '("hello"))

;; 边界测试 - 已排序列表
(check (($ '(1 2 3 4 5) :sort-by (lambda (x) x)) :collect) => '(1 2 3 4 5))
(check (($ '("a" "bb" "ccc") :sort-by string-length) :collect) => '("a" "bb" "ccc"))

;; 边界测试 - 稳定排序（保持相等键值元素的相对顺序）
(check (($ '((1 . "a") (2 . "b") (1 . "c") (3 . "d"))
          :sort-by car)
        :collect) => '((1 . "a") (1 . "c") (2 . "b") (3 . "d")))

;; 边界测试 - 复杂键提取函数
(check (($ '(10 2 25 7 100) :sort-by (lambda (x) (modulo x 10))) :collect) => '(10 100 2 25 7))
(check (($ '("hello" "world" "test") :sort-by (lambda (s) (char->integer (string-ref s 0)))) :collect) => '("hello" "test" "world"))

;; 链式调用测试
(check (($ '(-5 2 -8 1 9) :sort-by abs :map (lambda (x) (* x 2))) :collect) => '(2 4 -10 -16 18))
(check (($ '(-10 3 -7 2 6) :sort-by abs :filter even?) :collect) => '(2 6 -10))
(check (($ '(-5 2 -8 1 9) :sort-by abs :take 3) :collect) => '(1 2 -5))

;; 验证不改变原列表
(let ((lst (rich-list '(-5 2 -8 1 9))))
  (lst :sort-by abs)
  (check (lst :collect) => '(-5 2 -8 1 9))
) ;let


#|
rich-list%group-by
使用分组函数将rich-list中的元素分组。

语法
----
(lst :group-by func)

参数
----
func : procedure
分组函数，接受一个元素并返回分组键值。

返回值
-----
以rich-hash-table形式返回分组结果，其中：
- 键：分组函数返回的键值
- 值：包含对应元素的rich-list

功能
----
将rich-list中的元素根据分组函数func的返回值进行分组。
每个分组包含具有相同键值的所有元素，保持元素的原始顺序。
返回的rich-hash-table对象可以进一步使用hash-table方法操作。

边界条件
--------
- 空列表：返回空的rich-hash-table
- 所有元素具有相同键值：返回单个分组
- 每个元素具有不同键值：返回与元素数量相同的分组
- 分组函数返回复杂键值：支持任意类型的键值

性能特征
--------
- 时间复杂度：O(n)，需要遍历整个列表
- 空间复杂度：O(n)，需要存储分组结果

兼容性
------
- 与rich-hash-table类型系统兼容
- 支持链式调用模式
- 与标准数据分组操作语义一致
|#

;; 基本测试 - 按奇偶性分组
(let ((result ($ '(1 2 3 4 5 6)
               :group-by (lambda (x) (if (even? x) 'even 'odd)))))
  (check ((result :get 'even) :get) => '(2 4 6))
  (check ((result :get 'odd) :get) => '(1 3 5))
) ;let

;; 基本测试 - 按字符串长度分组
(let ((result ($ '("a" "bb" "ccc" "dd" "e")
               :group-by string-length)))
  (check ((result :get 1) :get) => '("a" "e"))
  (check ((result :get 2) :get) => '("bb" "dd"))
  (check ((result :get 3) :get) => '("ccc"))
) ;let

;; 基本测试 - 按首字母分组
(let ((result ($ '("apple" "banana" "cherry" "apricot" "blueberry")
               :group-by (lambda (s) (string-ref s 0)))))
  (check ((result :get #\a) :get) => '("apple" "apricot"))
  (check ((result :get #\b) :get) => '("banana" "blueberry"))
  (check ((result :get #\c) :get) => '("cherry"))
) ;let

;; 边界测试 - 空列表
(let ((result ($ '() :group-by (lambda (x) x))))
  (check (result :count (lambda (x) #t)) => 0)
) ;let

;; 边界测试 - 每个元素具有不同键值
(let ((result ($ '(1 2 3 4 5) :group-by (lambda (x) x))))
  (check ((result :get 1) :get) => '(1))
  (check ((result :get 2) :get) => '(2))
  (check ((result :get 3) :get) => '(3))
  (check ((result :get 4) :get) => '(4))
  (check ((result :get 5) :get) => '(5))
) ;let

;; 边界测试 - 复杂键值类型
(let ((result ($ '(1 2 3) :group-by (lambda (x) (list x (* x 2))))))
  (check ((result :get '(1 2)) :get) => '(1))
  (check ((result :get '(2 4)) :get) => '(2))
  (check ((result :get '(3 6)) :get) => '(3))
) ;let

;; 链式调用测试
(let ((result ($ '(1 2 3 4 5 6) :filter even? :group-by (lambda (x) (if (> x 3) 'large 'small)))))
  (check ((result :get 'small) :get) => '(2))
  (check ((result :get 'large) :get) => '(4 6))
) ;let

(let ((result ($ '(1 2 3 4 5) :map (lambda (x) (* x 2)) :group-by (lambda (x) (if (even? x) 'even 'odd)))))
  (check ((result :get 'even) :get) => '(2 4 6 8 10))
) ;let

;; 验证保持元素顺序
(let ((result ($ '(3 1 4 1 5 9 2 6) :group-by (lambda (x) (if (even? x) 'even 'odd)))))
  (check ((result :get 'odd) :get) => '(3 1 1 5 9))
  (check ((result :get 'even) :get) => '(4 2 6))
) ;let

;; 验证不改变原列表
(let ((lst (rich-list '(1 2 3 4 5))))
  (lst :group-by even?)
  (check (lst :collect) => '(1 2 3 4 5))
) ;let


#|
rich-list%sliding
将rich-list分成指定大小的滑动窗口。

语法
----
(lst :sliding size)
(lst :sliding size step)

参数
----
size : integer
滑动窗口的大小，必须为正整数。

step : integer (可选)
滑动窗口的步长，必须为正整数。默认为1。

返回值
-----
以向量形式返回包含滑动窗口的向量，每个窗口是一个列表。

功能
----
将rich-list分成指定大小的滑动窗口，支持可选的步长参数。
- 当窗口大小大于列表长度且没有步长参数时：返回包含整个列表的单个窗口
- 当窗口大小大于列表长度且有步长参数时：返回尽可能多的窗口
- 当有步长参数时：窗口之间按步长移动
- 当没有步长参数时：窗口之间按1的步长移动

边界条件
--------
- 空列表：返回空向量
- size为负数或0：抛出value-error异常
- size不是整数：抛出type-error异常
- step为负数或0：抛出value-error异常
- step不是整数：抛出type-error异常
- 窗口大小大于列表长度且没有步长参数：返回包含整个列表的单个窗口
- 窗口大小大于列表长度且有步长参数：返回尽可能多的窗口

性能特征
--------
- 时间复杂度：O(n)，需要遍历列表
- 空间复杂度：O(n)，需要存储所有窗口

兼容性
------
- 与所有rich-list实例兼容
- 返回向量，可与向量操作兼容
|#

;; 基本测试 - 单参数调用（默认步长为1）
(check ($ '(1 2 3 4 5) :sliding 2) => (list->vector '((1 2) (2 3) (3 4) (4 5))))
(check ($ '(a b c d) :sliding 3) => (list->vector '((a b c) (b c d))))
(check ($ '(10 20 30) :sliding 1) => (list->vector '((10) (20) (30))))

;; 基本测试 - 双参数调用（指定步长）
(check ($ '(1 2 3 4 5) :sliding 2 2) => (list->vector '((1 2) (3 4) (5))))
(check ($ '(a b c d e) :sliding 3 2) => (list->vector '((a b c) (c d e) (e))))
(check ($ '(10 20 30 40) :sliding 2 3) => (list->vector '((10 20) (40))))

;; 边界测试 - 空列表
(check ($ '() :sliding 2 :collect) => (list->vector '()))
(check ($ '() :sliding 3 2 :collect) => (list->vector '()))

;; 边界测试 - 单元素列表
(check ($ '(42) :sliding 1) => (list->vector '((42))))
(check ($ '(42) :sliding 2) => (list->vector '((42))))
(check ($ '(42) :sliding 1 1) => (list->vector '((42))))

;; 边界测试 - 窗口大小等于列表长度
(check ($ '(1 2 3) :sliding 3) => (list->vector '((1 2 3))))
(check ($ '(a b c) :sliding 3 1) => (list->vector '((a b c) (b c) (c))))

;; 边界测试 - 窗口大小大于列表长度
(check ($ '(1 2) :sliding 3) => (list->vector '((1 2))))
(check ($ '(a b) :sliding 5 2) => (list->vector '((a b))))

;; 边界测试 - 步长等于窗口大小
(check ($ '(1 2 3 4 5) :sliding 2 2) => (list->vector '((1 2) (3 4) (5))))
(check ($ '(a b c d e) :sliding 3 3) => (list->vector '((a b c) (d e))))

;; 边界测试 - 步长大于窗口大小
(check ($ '(1 2 3 4 5 6) :sliding 2 3) => (list->vector '((1 2) (4 5))))
(check ($ '(a b c d e f) :sliding 2 4) => (list->vector '((a b) (e f))))

;; 边界测试 - 步长小于窗口大小
(check ($ '(1 2 3 4 5 6) :sliding 3 2) => (list->vector '((1 2 3) (3 4 5) (5 6))))
(check ($ '(a b c d e f) :sliding 4 2) => (list->vector '((a b c d) (c d e f) (e f))))

;; 边界测试 - 异常处理
(check-catch 'value-error ($ '(1 2 3) :sliding 0))
(check-catch 'value-error ($ '(1 2 3) :sliding -1))
(check-catch 'type-error ($ '(1 2 3) :sliding 1.5))
(check-catch 'value-error ($ '(1 2 3) :sliding 2 0))
(check-catch 'value-error ($ '(1 2 3) :sliding 2 -1))
(check-catch 'type-error ($ '(1 2 3) :sliding 2 1.5))

;; 链式调用测试
(check (vector-length ($ '(1 2 3 4 5) :sliding 2)) => 4)
(check (vector-length ($ '(1 2 3 4 5) :sliding 2 2)) => 3)

;; 验证不改变原列表
(let ((lst (rich-list '(1 2 3 4 5))))
  (lst :sliding 2)
  (check (lst :collect) => '(1 2 3 4 5))
) ;let

(check ($ '(1 2 3) :apply 0) => 1)
(check ($ '(1 2 3) 0) => 1)

(check ($ (list ($ 1) ($ 2) ($ 3))) => (($ 1 :to 3) :map $))

(check-report)
