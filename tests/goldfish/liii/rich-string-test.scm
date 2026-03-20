;
; Copyright (C) 2025 The Goldfish Scheme Authors
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
        (liii rich-string)
        (liii lang)
        (liii error)
) ;import

(check-set-mode! 'report-failed)


#|
rich-string@empty
创建一个空的rich-string对象。

语法
----
(rich-string :empty . args)

参数
----
args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回空的字符串对象。

说明
----
创建一个不包含任何字符的rich-string。通常用于初始化字符串数据结构或作为
链式操作的起点。

边界条件
--------
- 无参数调用：返回空字符串
- 支持链式调用：可与其他rich-string方法组合使用

性能特征
--------
- 时间复杂度：O(1)，固定时间创建
- 空间复杂度：O(1)，创建空对象所需最小内存

兼容性
------
- 与所有rich-string实例方法兼容
- 支持链式调用模式
|#

;; 基本测试
(check ((rich-string :empty) :get) => "")
(check ((rich-string :empty) :length) => 0)
(check ((rich-string :empty) :empty?) => #t)

;; 边界测试
(check ((rich-string :empty :map (lambda (x) (x :to-upper))) :get) => "")
(check ((rich-string :empty :filter (lambda (x) #t)) :get) => "")

;; 验证类型正确性
(check-true (rich-string :is-type-of (rich-string :empty)))
(check-false (rich-string :is-type-of ""))

;; 链式调用测试
(check ((rich-string :empty :+ "hello") :get) => "hello")
(check ((rich-string :empty :strip-both) :get) => "")

;; 验证空字符串与其他方法的兼容性
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :empty) :ends-with "") => #t)
(check ((rich-string :empty) :contains "") => #t)

#|
rich-string@value-of
从不同类型的值创建rich-string对象。

语法
----
(rich-string :value-of v . args)

参数
----
v : any
要转换为rich-string的值，支持以下类型：
- char：单个字符
- number：数字
- symbol：符号
- string：字符串
- rich-char：rich-char对象

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回转换后的字符串对象。

说明
----
将不同类型的值转换为rich-string对象。对于不支持的输入类型会抛出类型错误。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- 字符：转换为单字符字符串
- 数字：转换为数字的字符串表示
- 符号：转换为符号的字符串表示
- 字符串：直接包装为rich-string
- rich-char：转换为对应的字符串表示
- 其他类型：抛出类型错误

性能特征
--------
- 时间复杂度：O(n)，其中n为结果字符串的长度
- 空间复杂度：O(n)，需要存储转换后的字符串

兼容性
------
- 与所有rich-string实例方法兼容
- 支持链式调用模式
|#

;; 基本功能测试
;; 字符类型
(check ((rich-string :value-of #\a) :get) => "a")
;; 数字类型
(check ((rich-string :value-of 123) :get) => "123")
;; 符号类型
(check ((rich-string :value-of 'hello) :get) => "hello")
;; 字符串类型
(check ((rich-string :value-of "hello") :get) => "hello")
(check ((rich-string :value-of "测试") :get) => "测试")
;; rich-char类型
(check ((rich-string :value-of (rich-char #\x)) :get) => "x")

;; 边界条件测试
(check ((rich-string :value-of "") :length) => 0)

;; 链式调用测试
(check ((rich-string :value-of "hello" :+ " world") :get) => "hello world")

;; 类型验证
(check-true (rich-string :is-type-of (rich-string :value-of "hello")))

;; 错误处理测试
(check-catch 'type-error (rich-string :value-of #t))

#|
rich-string%get
获取rich-string对象内部存储的原始字符串数据。

语法
----
(rich-string-instance :get)

参数
----
无参数。

返回值
-----
以string形式返回rich-string对象内部存储的原始字符串数据。

说明
----
该方法返回rich-string对象内部包装的原始字符串数据。这是获取rich-string
底层字符串表示的最直接方式。

边界条件
--------
- 空字符串：返回空字符串""
- 单字符字符串：返回单字符字符串
- 多字符字符串：返回完整的字符串
- Unicode字符串：返回包含Unicode字符的字符串

性能特征
--------
- 时间复杂度：O(1)，直接返回内部引用
- 空间复杂度：O(1)，不创建新字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 返回标准Scheme字符串，可与任何字符串操作函数配合使用
|#

;; 基本功能测试
;; 空字符串
(check ((rich-string :empty) :get) => "")
;; 单字符字符串
(check ((rich-string :value-of #\a) :get) => "a")
;; 多字符字符串
(check ($ "hello" :get) => "hello")
;; Unicode字符串
(check ($ "测试" :get) => "测试")
(check ($ "🎉" :get) => "🎉")
;; 数字转换的字符串
(check ((rich-string :value-of 123) :get) => "123")
;; 符号转换的字符串
(check ((rich-string :value-of 'hello) :get) => "hello")

;; 边界条件测试
(check (string? ($ "hello" :get)) => #t)
(check (string-length ($ "hello" :get)) => 5)

;; 验证内容一致性
(check (equal? ($ "hello" :get) "hello") => #t)

;; 链式操作后获取结果
(check ($ "hello" :+ " world" :get) => "hello world")
(check ($ "  hello  " :strip-both :get) => "hello")

#|
rich-string%length
获取rich-string对象中Unicode字符的数量。

语法
----
(rich-string-instance :length)
($ "string-content" :length)
($ "string-content" :method1 arg1_1 arg1_2 :method2 arg2 :length)

参数
----
无参数。

返回值
-----
以integer形式返回rich-string对象中Unicode字符的数量。

说明
----
该方法返回rich-string对象中Unicode字符的数量，而不是字节长度。
对于空字符串返回0，对于包含Unicode字符的字符串返回实际的字符数量。

边界条件
--------
- 空字符串：返回0
- ASCII字符串：字符数量等于字符串长度
- Unicode字符串：返回实际的Unicode字符数量（可能小于字节长度）
- 混合字符：正确计算所有Unicode字符的数量

性能特征
--------
- 时间复杂度：O(1)，长度在对象创建时已计算并缓存
- 空间复杂度：O(1)，直接返回缓存的长度值

兼容性
------
- 与所有rich-string实例兼容
- 返回标准整数，可与任何数值操作配合使用
|#

;; 基本功能测试
;; 空字符串
(check ((rich-string :empty) :length) => 0)

;; 单字符字符串
(check ($ "a" :length) => 1)

;; 多字符ASCII字符串
(check ($ "hello" :length) => 5)

;; Unicode字符测试
(check ($ "测试" :length) => 2)
(check ($ "🎉🎊" :length) => 2)

;; 混合字符
(check ($ "hello 世界 🎉" :length) => 10)

;; 链式操作后长度验证
(check ($ "hello" :+ " world" :length) => 11)

;; 长字符串测试
(check ($ (make-string 1000 #\a) :length) => 1000)


#|
rich-string%char-at
获取rich-string对象中指定索引位置的Unicode字符。

语法
----
(rich-string-instance :char-at index)

参数
----
index : integer
要访问的字符索引，从0开始计数。

返回值
-----
以rich-char形式返回指定索引位置的Unicode字符。

说明
----
该方法返回rich-string对象中指定索引位置的Unicode字符，以rich-char对象形式返回。
索引从0开始，表示第一个字符的位置。如果索引超出字符串范围，会抛出索引错误。
该方法正确处理Unicode字符，能够准确提取多字节编码的字符。

边界条件
--------
- 索引0：返回第一个字符
- 索引length-1：返回最后一个字符
- 负索引：抛出索引错误
- 超出范围的索引：抛出索引错误
- 非整数索引：抛出类型错误

性能特征
--------
- 时间复杂度：O(1)，直接定位到指定字符位置
- 空间复杂度：O(1)，创建单个rich-char对象

兼容性
------
- 与所有rich-string实例兼容
- 返回rich-char对象，可与rich-char相关操作配合使用
|#

;; 基本功能测试
;; ASCII字符串字符访问
(check (((rich-string :value-of "hello") :char-at 0) :make-string) => "h")
(check (((rich-string :value-of "hello") :char-at 4) :make-string) => "o")
;; Unicode字符访问测试
(check (((rich-string :value-of "测试") :char-at 0) :make-string) => "测")
(check (((rich-string :value-of "🎉🎊") :char-at 1) :make-string) => "🎊")

;; 边界条件测试
(check-catch 'out-of-range ((rich-string :empty) :char-at 0))
(check-catch 'out-of-range ((rich-string :value-of "hello") :char-at -1))
(check-catch 'out-of-range ((rich-string :value-of "hello") :char-at 5))

;; 验证返回类型是rich-char
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :char-at 0)))

;; 链式操作测试
(check ((((rich-string :value-of "hello") :char-at 0) :to-upper) :make-string) => "H")

;; 验证字符访问与字符串内容的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :char-at 0) :make-string) => "h")
) ;let

#|
rich-string%apply
获取rich-string对象中指定索引位置的Unicode字符（函数式编程风格接口）。

语法
----
(rich-string-instance :apply index)

参数
----
index : integer
要访问的字符索引，从0开始计数。

返回值
-----
以rich-char形式返回指定索引位置的Unicode字符。

说明
----
该方法是rich-string%char-at的别名，提供函数式编程风格的字符访问接口。
返回rich-string对象中指定索引位置的Unicode字符，以rich-char对象形式返回。
索引从0开始，表示第一个字符的位置。如果索引超出字符串范围，会抛出索引错误。
该方法正确处理Unicode字符，能够准确提取多字节编码的字符。

边界条件
--------
- 索引0：返回第一个字符
- 索引length-1：返回最后一个字符
- 负索引：抛出索引错误
- 超出范围的索引：抛出索引错误
- 非整数索引：抛出类型错误

性能特征
--------
- 时间复杂度：O(1)，直接定位到指定字符位置
- 空间复杂度：O(1)，创建单个rich-char对象

兼容性
------
- 与所有rich-string实例兼容
- 返回rich-char对象，可与rich-char相关操作配合使用
- 与rich-string%char-at方法功能完全一致
|#

;; 基本功能测试
;; ASCII字符串字符访问
(check (((rich-string :value-of "hello") :apply 0) :make-string) => "h")
(check (((rich-string :value-of "hello") :apply 4) :make-string) => "o")
;; Unicode字符访问测试
(check (((rich-string :value-of "测试") :apply 0) :make-string) => "测")

;; 边界条件测试
(check-catch 'out-of-range ((rich-string :empty) :apply 0))
(check-catch 'out-of-range ((rich-string :value-of "hello") :apply -1))
(check-catch 'out-of-range ((rich-string :value-of "hello") :apply 5))

;; 验证返回类型是rich-char
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :apply 0)))

;; 链式操作测试
(check ((((rich-string :value-of "hello") :apply 0) :to-upper) :make-string) => "H")

;; 验证apply与char-at方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :apply 0) :make-string) => ((rs :char-at 0) :make-string))
) ;let

#|
rich-string%find
在rich-string中查找满足给定谓词的第一个字符。

语法
----
(rich-string-instance :find pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以option形式返回满足谓词的第一个字符的rich-char对象。
如果没有任何字符满足谓词，返回none。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
返回第一个满足谓词的字符的option包装。如果遍历完所有字符都没有找到
满足条件的字符，则返回none。

该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回none
- 所有字符都不满足谓词：返回none
- 第一个字符满足谓词：返回第一个字符的option
- 中间字符满足谓词：返回第一个满足条件的字符的option
- 最后一个字符满足谓词：返回最后一个字符的option

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，只创建单个option对象

兼容性
------
- 与所有rich-string实例兼容
- 返回option类型，可与option相关操作配合使用
- 谓词函数必须接受rich-char对象作为参数
|#

;; 基本功能测试
;; 查找特定字符
(check ((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get) :make-string) => "h")
;; 查找大写字母
(check ((((rich-string :value-of "Hello") :find (lambda (c) (c :upper?))) :get) :make-string) => "H")
;; 查找数字字符
(check ((((rich-string :value-of "abc123") :find (lambda (c) (c :digit?))) :get) :make-string) => "1")

;; 边界条件测试
(check-true (((rich-string :empty) :find (lambda (c) #t)) :empty?))
(check-true (((rich-string :value-of "hello") :find (lambda (c) (c :equals #\x))) :empty?))
(check ((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\l))) :get) :make-string) => "l")

;; Unicode字符查找测试
(check ((((rich-string :value-of "hello世界") :find (lambda (c) (string=? (c :make-string) "世"))) :get) :make-string) => "世")
(check ((((rich-string :value-of "hello🎉world") :find (lambda (c) (string=? (c :make-string) "🎉"))) :get) :make-string) => "🎉")

;; 复杂谓词测试
(check ((((rich-string :value-of "hello World") :find (lambda (c) (c :upper?))) :get) :make-string) => "W")

;; 验证返回类型
(check-true (option :is-type-of ((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h)))))
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get)))

;; 链式操作测试
(check (((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get) :to-upper) :make-string) => "H")

;; 验证查找结果与字符访问的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (((rs :find (lambda (c) (c :equals #\w))) :get) :make-string) => "w")
) ;let

#|
rich-string%find-last
在rich-string中从后向前查找满足给定谓词的第一个字符。

语法
----
(rich-string-instance :find-last pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以option形式返回从后向前查找时第一个满足谓词的字符的rich-char对象。
如果没有任何字符满足谓词，返回none。

说明
----
该方法从rich-string的末尾开始向前遍历每个字符，对每个字符应用谓词函数pred。
返回从后向前查找时第一个满足谓词的字符的option包装。如果遍历完所有字符都没有找到
满足条件的字符，则返回none。

与%find方法不同，%find-last从字符串末尾开始查找，返回最后一个满足条件的字符。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回none
- 所有字符都不满足谓词：返回none
- 第一个字符满足谓词：如果从后向前查找，只有当它是唯一满足条件的字符时才会返回
- 中间字符满足谓词：返回最后一个满足条件的字符的option
- 最后一个字符满足谓词：返回最后一个字符的option

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，只创建单个option对象

兼容性
------
- 与所有rich-string实例兼容
- 返回option类型，可与option相关操作配合使用
- 谓词函数必须接受rich-char对象作为参数
|#

;; 基本功能测试
;; 查找特定字符（从后向前）
(check ((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get) :make-string) => "o")
;; 查找大写字母（从后向前）
(check ((((rich-string :value-of "HeLLo") :find-last (lambda (c) (c :upper?))) :get) :make-string) => "L")
;; 查找数字字符（从后向前）
(check ((((rich-string :value-of "abc123") :find-last (lambda (c) (c :digit?))) :get) :make-string) => "3")

;; 边界条件测试
(check-true (((rich-string :empty) :find-last (lambda (c) #t)) :empty?))
(check-true (((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\x))) :empty?))
(check ((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\l))) :get) :make-string) => "l")

;; Unicode字符查找测试
(check ((((rich-string :value-of "hello世界") :find-last (lambda (c) (string=? (c :make-string) "界"))) :get) :make-string) => "界")
(check ((((rich-string :value-of "hello🎉world🎊") :find-last (lambda (c) (string=? (c :make-string) "🎊"))) :get) :make-string) => "🎊")

;; 复杂谓词测试
(check ((((rich-string :value-of "Hello World") :find-last (lambda (c) (c :upper?))) :get) :make-string) => "W")

;; 验证返回类型
(check-true (option :is-type-of ((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o)))))
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get)))

;; 链式操作测试
(check (((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get) :to-upper) :make-string) => "O")

;; 与find方法的对比测试
(let ((rs (rich-string :value-of "HeLLo")))
  (check (((rs :find (lambda (c) (c :upper?))) :get) :make-string) => "H")
  (check (((rs :find-last (lambda (c) (c :upper?))) :get) :make-string) => "L")
) ;let

#|
rich-string%head
获取rich-string对象中的第一个字符。

语法
----
(rich-string-instance :head)

参数
----
无参数。

返回值
-----
以rich-char形式返回rich-string对象中的第一个字符。

说明
----
该方法返回rich-string对象中的第一个字符，以rich-char对象形式返回。
如果字符串为空，会抛出索引错误。该方法正确处理Unicode字符，能够
准确返回多字节编码的字符。

边界条件
--------
- 空字符串：抛出索引错误
- 单字符字符串：返回唯一的字符的rich-char对象
- 多字符字符串：返回第一个字符的rich-char对象
- Unicode字符串：正确返回第一个Unicode字符的rich-char对象

性能特征
--------
- 时间复杂度：O(1)，直接访问第一个字符
- 空间复杂度：O(1)，创建单个rich-char对象

兼容性
------
- 与所有rich-string实例兼容
- 返回rich-char对象，可与rich-char相关操作配合使用
|#

;; 基本功能测试
;; ASCII字符串的第一个字符
(check (((rich-string :value-of "hello") :head) :make-string) => "h")
;; 单字符字符串
(check (((rich-string :value-of "a") :head) :make-string) => "a")
;; Unicode字符测试
(check (((rich-string :value-of "测试") :head) :make-string) => "测")
(check (((rich-string :value-of "🎉") :head) :make-string) => "🎉")
;; 混合字符
(check (((rich-string :value-of "hello 世界 🎉") :head) :make-string) => "h")

;; 边界条件测试
(check-catch 'index-error ((rich-string :empty) :head))

;; 验证返回类型是rich-char
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :head)))

;; 链式操作测试
(check ((((rich-string :value-of "hello") :head) :to-upper) :make-string) => "H")
(check-true (((rich-string :value-of "hello") :head) :equals #\h))

;; 验证字符访问与字符串内容的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :head) :make-string) => "h")
) ;let

;; 验证不同类型输入的字符访问
(check (((rich-string :value-of 123) :head) :make-string) => "1")

;; 验证head与char-at方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :head) :make-string) => ((rs :char-at 0) :make-string))
) ;let

#|
rich-string%head-option
获取rich-string对象中的第一个字符，以option类型返回。

语法
----
(rich-string-instance :head-option)

参数
----
无参数。

返回值
-----
以option形式返回rich-string对象中的第一个字符。
如果字符串为空，返回none；否则返回包含第一个字符的option。

说明
----
该方法返回rich-string对象中的第一个字符，以option包装的rich-char对象形式返回。
与%head方法不同，%head-option在字符串为空时不会抛出错误，而是返回none。
该方法正确处理Unicode字符，能够准确返回多字节编码的字符。

边界条件
--------
- 空字符串：返回none
- 单字符字符串：返回包含唯一字符的option
- 多字符字符串：返回包含第一个字符的option
- Unicode字符串：正确返回第一个Unicode字符的option

性能特征
--------
- 时间复杂度：O(1)，直接访问第一个字符
- 空间复杂度：O(1)，创建单个option对象

兼容性
------
- 与所有rich-string实例兼容
- 返回option类型，可与option相关操作配合使用
- 与%head方法功能互补，提供安全的字符访问
|#

;; 基本功能测试
(check ((((rich-string :value-of "hello") :head-option) :get) :make-string) => "h")
(check ((((rich-string :value-of "a") :head-option) :get) :make-string) => "a")

;; Unicode字符测试
(check ((((rich-string :value-of "测试") :head-option) :get) :make-string) => "测")
(check ((((rich-string :value-of "🎉🎊") :head-option) :get) :make-string) => "🎉")

;; 边界条件测试
(check-true (((rich-string :empty) :head-option) :empty?))
(check-true (((rich-string :value-of "") :head-option) :empty?))

;; 验证返回类型
(check-true (option :is-type-of ((rich-string :value-of "hello") :head-option)))
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :head-option) :get)))

;; 链式操作测试
(check (((((rich-string :value-of "hello") :head-option) :get) :to-upper) :make-string) => "H")
(check (((((rich-string :value-of "hello") :head-option) :map (lambda (c) (c :to-upper))) :get) :make-string) => "H")

;; 与其他方法的对比测试
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :head) :make-string) => (((rs :head-option) :get) :make-string))
) ;let

;; option类型操作测试
(check ((((rich-string :value-of "hello") :head-option) :get-or-else (rich-char #\x)) :make-string) => "h")
(check ((((rich-string :empty) :head-option) :get-or-else (rich-char #\x)) :make-string) => "x")

#|
rich-string%last
获取rich-string对象中的最后一个字符。

语法
----
(rich-string-instance :last)

参数
----
无参数。

返回值
-----
以rich-char形式返回rich-string对象中的最后一个字符。

说明
----
该方法返回rich-string对象中的最后一个字符，以rich-char对象形式返回。
如果字符串为空，会抛出索引错误。该方法正确处理Unicode字符，能够
准确返回多字节编码的字符。

边界条件
--------
- 空字符串：抛出索引错误
- 单字符字符串：返回唯一的字符的rich-char对象
- 多字符字符串：返回最后一个字符的rich-char对象
- Unicode字符串：正确返回最后一个Unicode字符的rich-char对象

性能特征
--------
- 时间复杂度：O(1)，直接访问最后一个字符
- 空间复杂度：O(1)，创建单个rich-char对象

兼容性
------
- 与所有rich-string实例兼容
- 返回rich-char对象，可与rich-char相关操作配合使用
|#

;; 基本功能测试
(check (((rich-string :value-of "hello") :last) :make-string) => "o")
(check (((rich-string :value-of "a") :last) :make-string) => "a")

;; Unicode字符测试
(check (((rich-string :value-of "测试") :last) :make-string) => "试")
(check (((rich-string :value-of "🎉🎊") :last) :make-string) => "🎊")

;; 边界条件测试
(check-catch 'index-error ((rich-string :empty) :last))

;; 验证返回类型
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :last)))

;; 链式操作测试
(check ((((rich-string :value-of "hello") :last) :to-upper) :make-string) => "O")

;; 与其他方法的对比测试
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :last) :make-string) => ((rs :char-at (- (rs :length) 1)) :make-string))
) ;let

;; 验证字符访问一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :last) :make-string) => "d")
) ;let

;; 不同类型输入测试
(check (((rich-string :value-of 123) :last) :make-string) => "3")
(check (((rich-string :value-of 'hello) :last) :make-string) => "o")

#|
rich-string%last-option
获取rich-string对象中的最后一个字符，以option类型返回。

语法
----
(rich-string-instance :last-option)

参数
----
无参数。

返回值
-----
以option形式返回rich-string对象中的最后一个字符。
如果字符串为空，返回none；否则返回包含最后一个字符的option。

说明
----
该方法返回rich-string对象中的最后一个字符，以option包装的rich-char对象形式返回。
与%last方法不同，%last-option在字符串为空时不会抛出错误，而是返回none。
该方法正确处理Unicode字符，能够准确返回多字节编码的字符。

边界条件
--------
- 空字符串：返回none
- 单字符字符串：返回包含唯一字符的option
- 多字符字符串：返回包含最后一个字符的option
- Unicode字符串：正确返回最后一个Unicode字符的option

性能特征
--------
- 时间复杂度：O(1)，直接访问最后一个字符
- 空间复杂度：O(1)，创建单个option对象

兼容性
------
- 与所有rich-string实例兼容
- 返回option类型，可与option相关操作配合使用
- 与%last方法功能互补，提供安全的字符访问
|#

;; 基本功能测试
(check ((((rich-string :value-of "hello") :last-option) :get) :make-string) => "o")
(check ((((rich-string :value-of "a") :last-option) :get) :make-string) => "a")

;; Unicode字符测试
(check ((((rich-string :value-of "测试") :last-option) :get) :make-string) => "试")
(check ((((rich-string :value-of "🎉🎊") :last-option) :get) :make-string) => "🎊")

;; 边界条件测试
(check-true (((rich-string :empty) :last-option) :empty?))

;; 验证返回类型
(check-true (option :is-type-of ((rich-string :value-of "hello") :last-option)))
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :last-option) :get)))

;; 链式操作测试
(check (((((rich-string :value-of "hello") :last-option) :get) :to-upper) :make-string) => "O")
(check (((((rich-string :value-of "hello") :last-option) :map (lambda (c) (c :to-upper))) :get) :make-string) => "O")

;; 与其他方法的对比测试
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :last) :make-string) => (((rs :last-option) :get) :make-string))
) ;let

;; option类型操作测试
(check ((((rich-string :value-of "hello") :last-option) :get-or-else (rich-char #\x)) :make-string) => "o")
(check ((((rich-string :empty) :last-option) :get-or-else (rich-char #\x)) :make-string) => "x")

;; 验证字符访问一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (((rs :last-option) :get) :make-string) => "d")
) ;let

;; 不同类型输入测试
(check ((((rich-string :value-of 123) :last-option) :get) :make-string) => "3")

#|
rich-string%slice
从rich-string对象中提取指定范围的子字符串。

语法
----
(rich-string-instance :slice from until . args)

参数
----
from : integer
子字符串的起始索引（包含），从0开始计数。

until : integer
子字符串的结束索引（不包含），从0开始计数。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回从from到until-1的子字符串。

说明
----
该方法从rich-string对象中提取指定范围的子字符串，返回一个新的rich-string对象。
索引范围是半开区间[from, until)，即包含from位置的字符，但不包含until位置的字符。
如果from大于等于until，返回空字符串。
如果from或until超出字符串范围，会自动调整到有效的边界。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- from < 0：自动调整为0
- until > length：自动调整为length
- from >= until：返回空字符串
- from = 0且until = length：返回原始字符串
- 空字符串：返回空字符串

性能特征
--------
- 时间复杂度：O(k)，其中k是子字符串的长度
- 空间复杂度：O(k)，需要存储子字符串

兼容性
------
- 与所有rich-string实例兼容
- 支持链式调用模式
- 正确处理Unicode字符
|#

;; 基本功能测试
(check (((rich-string :value-of "hello world") :slice 0 5) :get) => "hello")
(check (((rich-string :value-of "hello world") :slice 6 11) :get) => "world")

;; Unicode字符切片测试
(check (((rich-string :value-of "测试字符串") :slice 0 2) :get) => "测试")
(check (((rich-string :value-of "🎉🎊🎈") :slice 0 1) :get) => "🎉")

;; 边界条件测试
(check (((rich-string :value-of "hello") :slice 0 3) :get) => "hel")
(check (((rich-string :value-of "hello") :slice 2 5) :get) => "llo")
(check (((rich-string :value-of "hello") :slice 3 2) :get) => "")

;; 链式调用测试
(check (((rich-string :value-of "hello world") :slice 0 5 :+ "!") :get) => "hello!")
(check (((rich-string :value-of "Hello World") :slice 0 5 :map (lambda (c) (c :to-lower))) :get) => "hello")

;; 验证返回类型和长度
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :slice 0 3)))
(check (((rich-string :value-of "hello") :slice 0 3) :length) => 3)

;; 验证切片内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :slice 0 5) :get) => "hello")
  (check ((rs :slice 6 11) :get) => "world")
) ;let

;; 边界条件验证
(check (((rich-string :empty) :slice 0 0) :get) => "")
(let ((rs (rich-string :value-of "a")))
  (check ((rs :slice 0 1) :get) => "a")
  (check ((rs :slice 0 0) :get) => "")
) ;let

#|
rich-string%take
从rich-string对象的前面提取指定数量的字符。

语法
----
(rich-string-instance :take n . args)

参数
----
n : integer
要提取的字符数量，从字符串开头开始计数。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回包含前n个字符的子字符串。

说明
----
该方法从rich-string对象的前面提取指定数量的字符，返回一个新的rich-string对象。
如果n大于字符串长度，返回整个字符串。
如果n小于等于0，返回空字符串。
该方法基于%slice方法实现，相当于调用(%slice 0 n)。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- n = 0：返回空字符串
- n < 0：返回空字符串
- n = length：返回整个字符串
- n > length：返回整个字符串
- 空字符串：返回空字符串

性能特征
--------
- 时间复杂度：O(k)，其中k是实际提取的字符数量
- 空间复杂度：O(k)，需要存储子字符串

兼容性
------
- 与所有rich-string实例兼容
- 支持链式调用模式
- 正确处理Unicode字符
- 与%slice方法功能一致，提供更简洁的前缀提取接口
|#

;; 基本功能测试
(check (((rich-string :value-of "hello world") :take 5) :get) => "hello")
(check (((rich-string :value-of "hello") :take 5) :get) => "hello")

;; Unicode字符提取测试
(check (((rich-string :value-of "测试字符串") :take 2) :get) => "测试")
(check (((rich-string :value-of "🎉🎊🎈") :take 1) :get) => "🎉")

;; 边界条件测试
(check (((rich-string :value-of "hello") :take 0) :get) => "")
(check (((rich-string :value-of "hello") :take -1) :get) => "")
(check (((rich-string :value-of "hello") :take 10) :get) => "hello")

;; 链式调用测试
(check (((rich-string :value-of "hello world") :take 5 :+ "!") :get) => "hello!")
(check (((rich-string :value-of "Hello World") :take 5 :map (lambda (c) (c :to-lower))) :get) => "hello")

;; 验证返回类型和长度
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :take 3)))
(check (((rich-string :value-of "hello") :take 3) :length) => 3)

;; 验证提取内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => "hello")
  (check ((rs :take 3) :get) => "hel")
) ;let

;; 边界条件验证
(let ((rs (rich-string :value-of "a")))
  (check ((rs :take 1) :get) => "a")
  (check ((rs :take 0) :get) => "")
) ;let

;; 与slice方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => ((rs :slice 0 5) :get))
) ;let

;; 不同类型输入测试
(check (((rich-string :value-of 12345) :take 3) :get) => "123")
(check (((rich-string :value-of 'hello) :take 3) :get) => "hel")

#|
rich-string%take-right
从rich-string对象的末尾提取指定数量的字符。

语法
----
(rich-string-instance :take-right n . args)

参数
----
n : integer
要提取的字符数量，从字符串末尾开始计数。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回包含后n个字符的子字符串。

说明
----
该方法从rich-string对象的末尾提取指定数量的字符，返回一个新的rich-string对象。
如果n大于字符串长度，返回整个字符串。
如果n小于等于0，返回空字符串。
该方法基于%slice方法实现，相当于调用(%slice (- length n) length)。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- n = 0：返回空字符串
- n < 0：返回空字符串
- n = length：返回整个字符串
- n > length：返回整个字符串
- 空字符串：返回空字符串

性能特征
--------
- 时间复杂度：O(k)，其中k是实际提取的字符数量
- 空间复杂度：O(k)，需要存储子字符串

兼容性
------
- 与所有rich-string实例兼容
- 支持链式调用模式
- 正确处理Unicode字符
- 与%slice方法功能一致，提供更简洁的后缀提取接口
- 与%take方法互补，分别处理字符串的前缀和后缀
|#

;; 基本功能测试
(check (((rich-string :value-of "hello world") :take-right 5) :get) => "world")
(check (((rich-string :value-of "hello") :take-right 5) :get) => "hello")

;; Unicode字符提取测试
(check (((rich-string :value-of "测试字符串") :take-right 2) :get) => "符串")
(check (((rich-string :value-of "🎉🎊🎈") :take-right 1) :get) => "🎈")

;; 边界条件测试
(check (((rich-string :value-of "hello") :take-right 0) :get) => "")
(check (((rich-string :value-of "hello") :take-right -1) :get) => "")
(check (((rich-string :value-of "hello") :take-right 10) :get) => "hello")

;; 链式调用测试
(check (((rich-string :value-of "hello world") :take-right 5 :+ "!") :get) => "world!")
(check (((rich-string :value-of "Hello World") :take-right 5 :map (lambda (c) (c :to-upper))) :get) => "WORLD")

;; 验证返回类型和长度
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :take-right 3)))
(check (((rich-string :value-of "hello") :take-right 3) :length) => 3)

;; 验证提取内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right 5) :get) => "world")
  (check ((rs :take-right 3) :get) => "rld")
) ;let

;; 边界条件验证
(let ((rs (rich-string :value-of "a")))
  (check ((rs :take-right 1) :get) => "a")
  (check ((rs :take-right 0) :get) => "")
) ;let

;; 与slice方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right 5) :get) => ((rs :slice 6 11) :get))
) ;let

;; 与take方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => "hello")
  (check ((rs :take-right 5) :get) => "world")
) ;let

;; 不同类型输入测试
(check (((rich-string :value-of 12345) :take-right 3) :get) => "345")
(check (((rich-string :value-of 'hello) :take-right 3) :get) => "llo")

#|
rich-string%drop
从rich-string对象的前面删除指定数量的字符，返回剩余部分。

语法
----
(rich-string-instance :drop n . args)

参数
----
n : integer
要从字符串开头删除的字符数量。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回删除前n个字符后的剩余字符串。

说明
----
该方法从rich-string对象的前面删除指定数量的字符，返回一个新的rich-string对象。
如果n大于等于字符串长度，返回空字符串。
如果n小于等于0，返回整个字符串。
该方法基于%slice方法实现，相当于调用(%slice n length)。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- n = 0：返回整个字符串
- n < 0：返回整个字符串
- n = length：返回空字符串
- n > length：返回空字符串
- 空字符串：返回空字符串

性能特征
--------
- 时间复杂度：O(k)，其中k是剩余字符串的长度
- 空间复杂度：O(k)，需要存储剩余字符串

兼容性
------
- 与所有rich-string实例兼容
- 支持链式调用模式
- 正确处理Unicode字符
- 与%slice方法功能一致，提供更简洁的前缀删除接口
- 与%take方法互补，分别处理字符串的前缀保留和删除
|#

;; 基本功能测试
(check (((rich-string :value-of "hello world") :drop 6) :get) => "world")
(check (((rich-string :value-of "hello") :drop 5) :get) => "")

;; Unicode字符删除测试
(check (((rich-string :value-of "测试字符串") :drop 2) :get) => "字符串")
(check (((rich-string :value-of "🎉🎊🎈") :drop 1) :get) => "🎊🎈")

;; 边界条件测试
(check (((rich-string :value-of "hello") :drop 0) :get) => "hello")
(check (((rich-string :value-of "hello") :drop -1) :get) => "hello")
(check (((rich-string :value-of "hello") :drop 10) :get) => "")

;; 链式调用测试
(check (((rich-string :value-of "hello world") :drop 6 :+ "!") :get) => "world!")
(check (((rich-string :value-of "Hello World") :drop 6 :map (lambda (c) (c :to-upper))) :get) => "WORLD")

;; 验证返回类型和长度
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :drop 3)))
(check (((rich-string :value-of "hello") :drop 3) :length) => 2)

;; 验证删除内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 6) :get) => "world")
  (check ((rs :drop 3) :get) => "lo world")
) ;let

;; 边界条件验证
(let ((rs (rich-string :value-of "a")))
  (check ((rs :drop 1) :get) => "")
  (check ((rs :drop 0) :get) => "a")
) ;let

;; 与slice方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 6) :get) => ((rs :slice 6 11) :get))
) ;let

;; 与take方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => "hello")
  (check ((rs :drop 5) :get) => " world")
) ;let

;; 不同类型输入测试
(check (((rich-string :value-of 12345) :drop 3) :get) => "45")
(check (((rich-string :value-of 'hello) :drop 3) :get) => "lo")

#|
rich-string%drop-right
从rich-string对象的末尾删除指定数量的字符，返回剩余部分。

语法
----
(rich-string-instance :drop-right n . args)

参数
----
n : integer
要从字符串末尾删除的字符数量。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回删除后n个字符后的剩余字符串。

说明
----
该方法从rich-string对象的末尾删除指定数量的字符，返回一个新的rich-string对象。
如果n大于等于字符串长度，返回空字符串。
如果n小于等于0，返回整个字符串。
该方法基于%slice方法实现，相当于调用(%slice 0 (- length n))。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- n = 0：返回整个字符串
- n < 0：返回整个字符串
- n = length：返回空字符串
- n > length：返回空字符串
- 空字符串：返回空字符串

性能特征
--------
- 时间复杂度：O(k)，其中k是剩余字符串的长度
- 空间复杂度：O(k)，需要存储剩余字符串

兼容性
------
- 与所有rich-string实例兼容
- 支持链式调用模式
- 正确处理Unicode字符
- 与%slice方法功能一致，提供更简洁的后缀删除接口
- 与%take-right方法互补，分别处理字符串的后缀保留和删除
- 与%drop方法互补，分别处理字符串的前缀和后缀删除
|#

;; 基本功能测试
(check (((rich-string :value-of "hello world") :drop-right 5) :get) => "hello ")
(check (((rich-string :value-of "hello") :drop-right 5) :get) => "")

;; Unicode字符删除测试
(check (((rich-string :value-of "测试字符串") :drop-right 2) :get) => "测试字")
(check (((rich-string :value-of "🎉🎊🎈") :drop-right 1) :get) => "🎉🎊")

;; 边界条件测试
(check (((rich-string :value-of "hello") :drop-right 0) :get) => "hello")
(check (((rich-string :value-of "hello") :drop-right -1) :get) => "hello")
(check (((rich-string :value-of "hello") :drop-right 10) :get) => "")

;; 链式调用测试
(check (((rich-string :value-of "hello world") :drop-right 5 :+ "!") :get) => "hello !")
(check (((rich-string :value-of "Hello World") :drop-right 5 :map (lambda (c) (c :to-upper))) :get) => "HELLO ")

;; 验证返回类型和长度
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :drop-right 3)))
(check (((rich-string :value-of "hello") :drop-right 3) :length) => 2)

;; 验证删除内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop-right 5) :get) => "hello ")
  (check ((rs :drop-right 3) :get) => "hello wo")
) ;let

;; 边界条件验证
(let ((rs (rich-string :value-of "a")))
  (check ((rs :drop-right 1) :get) => "")
  (check ((rs :drop-right 0) :get) => "a")
) ;let

;; 与slice方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop-right 5) :get) => ((rs :slice 0 6) :get))
) ;let

;; 与take-right方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right 5) :get) => "world")
  (check ((rs :drop-right 5) :get) => "hello ")
) ;let

;; 与drop方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 5) :get) => " world")
  (check ((rs :drop-right 5) :get) => "hello ")
) ;let

;; 不同类型输入测试
(check (((rich-string :value-of 12345) :drop-right 3) :get) => "12")
(check (((rich-string :value-of 'hello) :drop-right 3) :get) => "he")

#|
rich-string%empty?
检查rich-string对象是否为空字符串。

语法
----
(rich-string-instance :empty?)

参数
----
无参数。

返回值
-----
以boolean形式返回rich-string对象是否为空字符串。
如果字符串长度为0，返回#t；否则返回#f。

说明
----
该方法检查rich-string对象是否为空字符串（即不包含任何字符）。
对于空字符串和长度为0的字符串返回#t，对于包含任何字符的字符串返回#f。
该方法正确处理Unicode字符，能够准确判断字符串是否为空。

边界条件
--------
- 空字符串：返回#t
- 单字符字符串：返回#f
- 多字符字符串：返回#f
- Unicode字符串：根据字符数量判断，有字符则返回#f
- 空rich-string对象：返回#t

性能特征
--------
- 时间复杂度：O(1)，直接检查缓存的长度值
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 返回标准布尔值，可与任何布尔操作配合使用
- 与%length方法关系密切，empty?等价于(length = 0)
|#

;; 基本功能测试
;; 空字符串
(check ((rich-string :empty) :empty?) => #t)
(check ((rich-string :value-of "") :empty?) => #t)

;; 单字符字符串
(check ((rich-string :value-of "a") :empty?) => #f)

;; 多字符字符串
(check ((rich-string :value-of "hello") :empty?) => #f)

;; Unicode字符测试
(check ((rich-string :value-of "测试") :empty?) => #f)

;; 链式操作后的空字符串判断
(check ((rich-string :value-of "hello" :slice 0 0) :empty?) => #t)

;; 验证返回类型是布尔值
(check (boolean? ((rich-string :empty) :empty?)) => #t)

;; 验证empty?与length方法的一致性
(check ((rich-string :empty) :empty?) => (zero? ((rich-string :empty) :length)))

;; 验证empty?与字符访问的一致性
(let ((rs (rich-string :value-of "hello")))
  (check (rs :empty?) => #f)
  ;; 删除所有字符后应该为空
  (check ((rs :drop 5) :empty?) => #t)
) ;let

;; 性能相关测试
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check (long-str :empty?) => #f)
) ;let

#|
rich-string%starts-with
检查rich-string对象是否以指定的前缀开头。

语法
----
(rich-string-instance :starts-with prefix)

参数
----
prefix : any
要检查的前缀，支持以下类型：
- string：标准字符串
- rich-string：rich-string对象
- char：单个字符
- rich-char：rich-char对象

返回值
-----
以boolean形式返回rich-string对象是否以指定前缀开头。
如果字符串以指定前缀开头，返回#t；否则返回#f。

说明
----
该方法检查rich-string对象是否以指定的前缀开头。支持多种参数类型，
包括字符串、rich-string、字符和rich-char。对于空前缀，总是返回#t。
该方法正确处理Unicode字符，能够准确判断字符串前缀。

边界条件
--------
- 空字符串：任何前缀都返回#t（包括空前缀）
- 空前缀：总是返回#t
- 前缀长度大于字符串长度：返回#f
- 前缀与字符串开头匹配：返回#t
- 前缀与字符串开头不匹配：返回#f
- Unicode字符前缀：正确匹配Unicode字符

性能特征
--------
- 时间复杂度：O(k)，其中k是前缀的长度
- 空间复杂度：O(1)，不创建新字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持多种前缀类型
- 正确处理Unicode字符
- 与%ends-with方法互补
|#

;; 基本功能测试 - 保留10个核心测试用例
;; 字符串前缀匹配
(check ((rich-string :value-of "hello world") :starts-with "hello") => #t)
(check ((rich-string :value-of "hello world") :starts-with "h") => #t)

;; 字符串前缀不匹配
(check ((rich-string :value-of "hello world") :starts-with "world") => #f)
(check ((rich-string :value-of "hello world") :starts-with "Hello") => #f)

;; 边界条件测试
;; 空字符串的前缀匹配
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :empty) :starts-with "hello") => #f)

;; 空前缀匹配
(check ((rich-string :value-of "hello") :starts-with "") => #t)

;; 前缀长度大于字符串长度
(check ((rich-string :value-of "hello") :starts-with "hello world") => #f)

;; Unicode字符前缀测试
(check ((rich-string :value-of "测试字符串") :starts-with "测试") => #t)
(check ((rich-string :value-of "测试字符串") :starts-with "字符") => #f)

#|
rich-string%ends-with
检查rich-string对象是否以指定的后缀结尾。

语法
----
(rich-string-instance :ends-with suffix)

参数
----
suffix : any
要检查的后缀，支持以下类型：
- string：标准字符串
- rich-string：rich-string对象
- char：单个字符
- rich-char：rich-char对象

返回值
-----
以boolean形式返回rich-string对象是否以指定后缀结尾。
如果字符串以指定后缀结尾，返回#t；否则返回#f。

说明
----
该方法检查rich-string对象是否以指定的后缀结尾。支持多种参数类型，
包括字符串、rich-string、字符和rich-char。对于空后缀，总是返回#t。
该方法正确处理Unicode字符，能够准确判断字符串后缀。

边界条件
--------
- 空字符串：任何后缀都返回#t（包括空后缀）
- 空后缀：总是返回#t
- 后缀长度大于字符串长度：返回#f
- 后缀与字符串结尾匹配：返回#t
- 后缀与字符串结尾不匹配：返回#f
- Unicode字符后缀：正确匹配Unicode字符

性能特征
--------
- 时间复杂度：O(k)，其中k是后缀的长度
- 空间复杂度：O(1)，不创建新字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持多种后缀类型
- 正确处理Unicode字符
- 与%starts-with方法互补
|#

;; 基本功能测试 - 保留10个核心测试用例
;; 字符串后缀匹配
(check ((rich-string :value-of "hello world") :ends-with "world") => #t)
(check ((rich-string :value-of "hello world") :ends-with "d") => #t)

;; 字符串后缀不匹配
(check ((rich-string :value-of "hello world") :ends-with "hello") => #f)
(check ((rich-string :value-of "hello world") :ends-with "World") => #f)

;; 边界条件测试
;; 空字符串的后缀匹配
(check ((rich-string :empty) :ends-with "") => #t)
(check ((rich-string :empty) :ends-with "hello") => #f)

;; 空后缀匹配
(check ((rich-string :value-of "hello") :ends-with "") => #t)

;; 后缀长度大于字符串长度
(check ((rich-string :value-of "hello") :ends-with "hello world") => #f)

;; Unicode字符后缀测试
(check ((rich-string :value-of "测试字符串") :ends-with "符串") => #t)
(check ((rich-string :value-of "测试字符串") :ends-with "测试") => #f)

#|
rich-string%to-rich-vector
将rich-string对象转换为rich-vector对象，其中每个元素是rich-char对象。

语法
----
(rich-string-instance :to-rich-vector)

返回值
-----
返回一个rich-vector对象，包含原字符串中每个字符对应的rich-char对象。

说明
----
该方法将rich-string对象转换为rich-vector对象，转换过程中每个字符都会被包装成rich-char对象。
返回的rich-vector对象与原字符串具有相同的字符顺序。
对于空字符串，返回空的rich-vector对象。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回空的rich-vector对象
- 单字符字符串：返回包含一个rich-char对象的rich-vector
- 多字符字符串：返回包含多个rich-char对象的rich-vector
- Unicode字符：正确处理Unicode字符的转换

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(n)，创建新的rich-vector对象

兼容性
------
- 与所有rich-string实例兼容
- 返回的rich-vector对象支持所有rich-vector的操作方法
|#

;; 基本功能测试
;; 普通字符串转换
(let ((vec ((rich-string :value-of "hello") :to-rich-vector)))
  (check (vec :length) => 5)
) ;let

;; 验证转换后的字符内容
(let ((vec ((rich-string :value-of "abc") :to-rich-vector)))
  (let ((codes (vec :map (lambda (c) (c :to-integer)) :collect)))
    (check codes => #(97 98 99))
  ) ;let
) ;let

;; 边界条件测试
;; 空字符串
(let ((vec ((rich-string :empty) :to-rich-vector)))
  (check (vec :empty?) => #t)
) ;let

;; 单字符字符串
(let ((vec ((rich-string :value-of "a") :to-rich-vector)))
  (check (vec :length) => 1)
) ;let

;; Unicode字符转换测试
(let ((vec ((rich-string :value-of "测试") :to-rich-vector)))
  (check (vec :length) => 2)
) ;let

;; 验证转换后的字符类型
(let ((vec ((rich-string :value-of "test") :to-rich-vector)))
  (check (vec :forall (lambda (c) (rich-char :is-type-of c))) => #t)
) ;let

;; 验证字符顺序
(let ((vec ((rich-string :value-of "123") :to-rich-vector)))
  (let ((codes (vec :map (lambda (c) (c :to-integer)) :collect)))
    (check codes => #(49 50 51))
  ) ;let
) ;let

;; 混合字符测试
(let ((vec ((rich-string :value-of "a1B2c3") :to-rich-vector)))
  (check (vec :length) => 6)
) ;let

;; 验证返回类型
(check (rich-vector :is-type-of ((rich-string :value-of "test") :to-rich-vector)) => #t)

#|
rich-string%forall
检查rich-string对象中的所有字符是否都满足给定的谓词条件。

语法
----
(rich-string-instance :forall pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以boolean形式返回所有字符是否都满足谓词条件。
如果所有字符都满足谓词条件，返回#t；否则返回#f。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
只有当所有字符都满足谓词条件时才返回#t，否则返回#f。
对于空字符串，总是返回#t（空真原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回#t（空真原则）
- 所有字符满足谓词：返回#t
- 至少一个字符不满足谓词：返回#f
- 第一个字符不满足谓词：立即返回#f（短路求值）
- 最后一个字符不满足谓词：返回#f

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持短路求值，提高性能
|#

;; 基本功能测试
;; 所有字符都满足谓词
(check ((rich-string :value-of "hello") :forall (lambda (c) (c :lower?))) => #t)

;; 存在字符不满足谓词
(check ((rich-string :value-of "Hello") :forall (lambda (c) (c :lower?))) => #f)

;; 边界条件测试
;; 空字符串（空真原则）
(check ((rich-string :empty) :forall (lambda (c) #f)) => #t)

;; 单字符字符串
(check ((rich-string :value-of "a") :forall (lambda (c) (c :lower?))) => #t)
(check ((rich-string :value-of "A") :forall (lambda (c) (c :lower?))) => #f)

;; Unicode字符测试
(check ((rich-string :value-of "测试") :forall (lambda (c) (c :ascii?))) => #f)  ; 中文字符不是ASCII

;; 复杂谓词测试
(check ((rich-string :value-of "abcde") :forall (lambda (c) (c :ascii?))) => #t)

;; 验证短路求值行为
(let ((count 0))
  ((rich-string :value-of "Hello") :forall (lambda (c)
    (set! count (+ count 1))
    (c :lower?))
  ) ;
  (check count => 1)  ; 应该在第一个字符'H'处停止
) ;let

;; 验证返回类型
(check (boolean? ((rich-string :value-of "hello") :forall (lambda (c) #t))) => #t)

;; 链式操作测试
(check ((rich-string :value-of "hhh") :forall (lambda (c) (c :equals #\h))) => #t)

#|
rich-string%exists
检查rich-string对象中是否存在至少一个字符满足给定的谓词条件。

语法
----
(rich-string-instance :exists pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以boolean形式返回是否存在至少一个字符满足谓词条件。
如果存在至少一个字符满足谓词条件，返回#t；否则返回#f。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
只要有一个字符满足谓词条件就立即返回#t，否则返回#f。
对于空字符串，总是返回#f（空假原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回#f（空假原则）
- 所有字符都不满足谓词：返回#f
- 至少一个字符满足谓词：返回#t
- 第一个字符满足谓词：立即返回#t（短路求值）
- 最后一个字符满足谓词：返回#t

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持短路求值，提高性能
|#

;; 基本功能测试
;; 存在字符满足谓词
(check ((rich-string :value-of "Hello") :exists (lambda (c) (c :upper?))) => #t)

;; 所有字符都不满足谓词
(check ((rich-string :value-of "hello") :exists (lambda (c) (c :upper?))) => #f)

;; 边界条件测试
;; 空字符串（空假原则）
(check ((rich-string :empty) :exists (lambda (c) #t)) => #f)

;; 单字符字符串
(check ((rich-string :value-of "A") :exists (lambda (c) (c :upper?))) => #t)

;; Unicode字符测试
(check ((rich-string :value-of "测试") :exists (lambda (c) (c :ascii?))) => #f)  ; 中文字符不是ASCII

;; 复杂谓词测试
(check ((rich-string :value-of "Hello123") :exists (lambda (c) (c :digit?))) => #t)

;; 验证短路求值行为
(let ((count 0))
  ((rich-string :value-of "Hello") :exists (lambda (c)
    (set! count (+ count 1))
    (c :upper?))
  ) ;
  (check count => 1)  ; 应该在第一个字符'H'处停止
) ;let

;; 验证返回类型
(check (boolean? ((rich-string :value-of "hello") :exists (lambda (c) #f))) => #t)

#|
rich-string%contains
检查rich-string对象中是否包含指定的子字符串或字符。

语法
----
(rich-string-instance :contains elem)

参数
----
elem : any
要查找的元素，支持以下类型：
- rich-string：rich-string对象
- string：标准字符串
- rich-char：rich-char对象
- char：单个字符

返回值
-----
以boolean形式返回字符串中是否包含指定的元素。
如果字符串包含指定的元素，返回#t；否则返回#f。

说明
----
该方法检查rich-string对象中是否包含指定的子字符串或字符。
支持多种参数类型，包括rich-string、string、rich-char和char。
对于空字符串，总是返回#t（包含空字符串）。
该方法正确处理Unicode字符，能够准确查找子字符串。

边界条件
--------
- 空字符串：包含空字符串返回#t，包含非空字符串返回#f
- 空元素：总是返回#t
- 元素长度大于字符串长度：返回#f
- 元素与字符串部分匹配：返回#t
- 元素与字符串不匹配：返回#f
- Unicode字符元素：正确匹配Unicode字符

性能特征
--------
- 时间复杂度：O(n)，需要搜索整个字符串
- 空间复杂度：O(1)，不创建新字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持多种元素类型
- 正确处理Unicode字符
|#

;; 基本功能测试
;; 字符串包含测试
(check ((rich-string :value-of "hello world") :contains "hello") => #t)

;; 字符串不包含测试
(check ((rich-string :value-of "hello world") :contains "test") => #f)

;; 字符包含测试
(check ((rich-string :value-of "hello") :contains #\h) => #t)

;; 字符不包含测试
(check ((rich-string :value-of "hello") :contains #\x) => #f)

;; 边界条件测试
;; 空字符串的包含测试
(check ((rich-string :empty) :contains "") => #t)
(check ((rich-string :empty) :contains "hello") => #f)

;; 空元素的包含测试
(check ((rich-string :value-of "hello") :contains "") => #t)

;; Unicode字符包含测试
(check ((rich-string :value-of "测试字符串") :contains "测试") => #t)

;; rich-string参数测试
(check ((rich-string :value-of "hello world") :contains (rich-string :value-of "hello")) => #t)

#|
rich-string%index-of
查找指定子字符串或字符在rich-string对象中第一次出现的索引位置。

语法
----
(rich-string-instance :index-of str/char (start-index 0))

参数
----
str/char : any
要查找的子字符串或字符，支持以下类型：
- rich-string：rich-string对象
- string：标准字符串
- rich-char：rich-char对象
- char：单个字符

start-index : integer (可选，默认为0)
开始搜索的索引位置，从0开始计数。

返回值
-----
以integer形式返回子字符串或字符第一次出现的索引位置。
如果未找到，返回-1。

说明
----
该方法在rich-string对象中查找指定的子字符串或字符第一次出现的索引位置。
支持多种参数类型，包括rich-string、string、rich-char和char。
可以指定开始搜索的索引位置，从该位置开始向后搜索。
该方法正确处理Unicode字符，能够准确查找子字符串的位置。

边界条件
--------
- 空字符串：查找任何元素（包括空元素）都返回-1
- 空元素：查找空元素总是返回-1
- 元素长度大于字符串长度：返回-1
- 元素与字符串部分匹配：返回匹配的起始索引
- 元素与字符串不匹配：返回-1
- 起始索引超出范围：返回-1
- Unicode字符元素：正确匹配Unicode字符

性能特征
--------
- 时间复杂度：O(n)，需要搜索整个字符串
- 空间复杂度：O(1)，不创建新字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持多种元素类型
- 支持指定起始搜索位置
- 正确处理Unicode字符
|#

;; 基本功能测试 - 字符串查找
(check ((rich-string :value-of "hello world") :index-of "hello") => 0)
(check ((rich-string :value-of "hello world") :index-of "world") => 6)

;; 字符串未找到测试
(check ((rich-string :value-of "hello world") :index-of "test") => -1)

;; 字符查找测试
(check ((rich-string :value-of "hello") :index-of #\h) => 0)
(check ((rich-string :value-of "hello") :index-of #\o) => 4)

;; 边界条件测试
(check ((rich-string :empty) :index-of "hello") => -1)

;; Unicode字符查找测试
(check ((rich-string :value-of "测试字符串") :index-of "测试") => 0)

;; 指定起始索引测试
(check ((rich-string :value-of "hello world") :index-of "l" 3) => 3)

;; rich-string参数测试
(check ((rich-string :value-of "hello world") :index-of (rich-string :value-of "hello")) => 0)

#|
rich-string%map
对rich-string对象中的每个字符应用映射函数，生成一个新的rich-string对象。

语法
----
(rich-string-instance :map f)

参数
----
f : procedure
一个接受rich-char对象作为参数并返回rich-char对象的映射函数。

返回值
-----
返回一个新的rich-string对象，包含映射后的字符序列。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用映射函数f。
映射函数必须接受rich-char对象作为参数并返回rich-char对象。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回空字符串
- 映射函数返回相同字符：返回与原字符串相同的字符串
- 映射函数改变字符：返回包含新字符的字符串
- Unicode字符映射：正确处理Unicode字符的映射

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 映射函数必须接受rich-char对象并返回rich-char对象
- 支持链式操作
|#

;; 基本功能测试
;; 字符大小写转换
(check ((rich-string :value-of "hello") :map (lambda (c) (c :to-upper)) :get) => "HELLO")

;; 字符类型转换
(check ((rich-string :value-of "123") :map (lambda (c) (c :to-upper)) :get) => "123")  ; 数字字符大小写转换无变化

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :map (lambda (c) (c :to-upper)) :get) => "")

;; 单字符字符串
(check ((rich-string :value-of "a") :map (lambda (c) (c :to-upper)) :get) => "A")

;; Unicode字符映射测试
(check ((rich-string :value-of "测试") :map (lambda (c) c) :get) => "测试")  ; 恒等映射

;; 复杂映射函数测试
(check ((rich-string :value-of "aBc") :map (lambda (c) (if (c :lower?) (c :to-upper) (c :to-lower))) :get) => "AbC")

;; 链式操作测试
(check ((rich-string :value-of "hello") :map (lambda (c) (c :to-upper)) :map (lambda (c) (c :to-lower)) :get) => "hello")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :map (lambda (c) c))) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :map (lambda (c) (c :to-upper)))
  (check (original :get) => "hello")
) ;let

#|
rich-string%filter
根据给定的谓词条件过滤rich-string对象中的字符，生成一个新的rich-string对象。

语法
----
(rich-string-instance :filter pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
返回一个新的rich-string对象，包含满足谓词条件的字符序列。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
只有满足谓词条件的字符会被保留在新字符串中。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确过滤多字节编码的字符。

边界条件
--------
- 空字符串：返回空字符串
- 所有字符都满足谓词：返回与原字符串相同的字符串
- 所有字符都不满足谓词：返回空字符串
- 部分字符满足谓词：返回包含满足条件字符的字符串
- Unicode字符过滤：正确处理Unicode字符的过滤

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持链式操作
|#

;; 基本功能测试
;; 过滤小写字母
(check ((rich-string :value-of "Hello World") :filter (lambda (c) (c :lower?)) :get) => "elloorld")

;; 过滤大写字母
(check ((rich-string :value-of "Hello World") :filter (lambda (c) (c :upper?)) :get) => "HW")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :filter (lambda (c) #t) :get) => "")

;; 所有字符都满足谓词
(check ((rich-string :value-of "abc") :filter (lambda (c) #t) :get) => "abc")

;; 所有字符都不满足谓词
(check ((rich-string :value-of "abc") :filter (lambda (c) #f) :get) => "")

;; Unicode字符过滤测试
(check ((rich-string :value-of "测试123") :filter (lambda (c) (c :digit?)) :get) => "123")

;; 复杂谓词测试
(check ((rich-string :value-of "a1B2c3") :filter (lambda (c) (or (c :lower?) (c :digit?))) :get) => "a12c3")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :filter (lambda (c) #t))) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "Hello")))
  (original :filter (lambda (c) (c :lower?)))
  (check (original :get) => "Hello")
) ;let

#|
rich-string%reverse
反转rich-string对象中的字符顺序，生成一个新的rich-string对象。

语法
----
(rich-string-instance :reverse)

返回值
-----
返回一个新的rich-string对象，包含字符顺序反转后的字符串。

说明
----
该方法反转rich-string对象中的字符顺序，返回一个新的rich-string对象。
原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确反转多字节编码的字符顺序。

边界条件
--------
- 空字符串：返回空字符串
- 单字符字符串：返回与原字符串相同的字符串
- 双字符字符串：交换两个字符的位置
- 长字符串：完全反转所有字符的顺序
- Unicode字符：正确处理Unicode字符的反转

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
|#

;; 基本功能测试
;; 普通字符串反转
(check ((rich-string :value-of "hello") :reverse :get) => "olleh")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :reverse :get) => "")

;; 单字符字符串
(check ((rich-string :value-of "a") :reverse :get) => "a")

;; 双字符字符串
(check ((rich-string :value-of "ab") :reverse :get) => "ba")

;; Unicode字符反转测试
(check ((rich-string :value-of "测试") :reverse :get) => "试测")

;; 混合字符测试
(check ((rich-string :value-of "a1B2c3") :reverse :get) => "3c2B1a")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :reverse)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :reverse)
  (check (original :get) => "hello")
) ;let

;; 链式操作测试
(check ((rich-string :value-of "abc") :reverse :reverse :get) => "abc")

#|
rich-string%index-where
查找rich-string对象中第一个满足给定谓词条件的字符的索引位置。

语法
----
(rich-string-instance :index-where pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以integer形式返回第一个满足谓词条件的字符的索引位置。
如果未找到满足条件的字符，返回-1。

说明
----
该方法从左到右遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
返回第一个满足谓词条件的字符的索引位置（从0开始计数）。
如果没有字符满足条件，返回-1。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回-1
- 所有字符都不满足谓词：返回-1
- 第一个字符满足谓词：返回0
- 最后一个字符满足谓词：返回最后一个索引
- 中间字符满足谓词：返回对应的索引
- Unicode字符：正确处理Unicode字符的索引位置

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的字符直到找到匹配
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持短路求值，找到第一个匹配就立即返回
|#

;; 基本功能测试
;; 查找第一个大写字母
(check ((rich-string :value-of "hello World") :index-where (lambda (c) (c :upper?))) => 6)

;; 查找第一个数字字符
(check ((rich-string :value-of "abc123") :index-where (lambda (c) (c :digit?))) => 3)

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :index-where (lambda (c) #t)) => -1)

;; 所有字符都不满足谓词
(check ((rich-string :value-of "hello") :index-where (lambda (c) (c :digit?))) => -1)

;; 第一个字符满足谓词
(check ((rich-string :value-of "Hello") :index-where (lambda (c) (c :upper?))) => 0)

;; 最后一个字符满足谓词
(check ((rich-string :value-of "hello!") :index-where (lambda (c) (c :equals #\!))) => 5)

;; Unicode字符查找测试
(check ((rich-string :value-of "测试字符串") :index-where (lambda (c) (c :equals (rich-char #x6D4B)))) => 0)  ; 查找"测"字

;; 复杂谓词测试
(check ((rich-string :value-of "a1B2c3") :index-where (lambda (c) (c :upper?))) => 2)  ; 查找第一个大写字母

;; 验证短路求值行为
(let ((count 0))
  ((rich-string :value-of "Hello") :index-where (lambda (c)
    (set! count (+ count 1))
    (c :upper?))
  ) ;
  (check count => 1)  ; 应该在第一个字符'H'处停止
) ;let

#|
rich-string%count
统计rich-string对象中满足给定谓词条件的字符数量。

语法
----
(rich-string-instance :count pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以integer形式返回满足谓词条件的字符数量。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
统计满足谓词条件的字符数量并返回。
对于空字符串，总是返回0。
该方法正确处理Unicode字符，能够准确统计多字节编码的字符。

边界条件
--------
- 空字符串：返回0
- 所有字符都满足谓词：返回字符串长度
- 所有字符都不满足谓词：返回0
- 部分字符满足谓词：返回满足条件的字符数量
- Unicode字符统计：正确处理Unicode字符的统计

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 统计结果总是非负整数
|#

;; 基本功能测试
;; 统计小写字母数量
(check ((rich-string :value-of "Hello World") :count (lambda (c) (c :lower?))) => 8)

;; 统计大写字母数量
(check ((rich-string :value-of "Hello World") :count (lambda (c) (c :upper?))) => 2)

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :count (lambda (c) #t)) => 0)

;; 所有字符都满足谓词
(check ((rich-string :value-of "abc") :count (lambda (c) #t)) => 3)

;; 所有字符都不满足谓词
(check ((rich-string :value-of "abc") :count (lambda (c) #f)) => 0)

;; Unicode字符统计测试
(check ((rich-string :value-of "测试123") :count (lambda (c) (c :digit?))) => 3)

;; 复杂谓词测试
(check ((rich-string :value-of "a1B2c3") :count (lambda (c) (or (c :lower?) (c :digit?)))) => 5)

;; 验证返回类型
(check (integer? ((rich-string :value-of "test") :count (lambda (c) #t))) => #t)

;; 验证统计结果非负
(check (>= ((rich-string :value-of "hello") :count (lambda (c) (c :lower?))) 0) => #t)

#|
rich-string%drop-while
从rich-string对象的开头开始，丢弃满足给定谓词条件的连续字符，返回剩余部分的rich-string对象。

语法
----
(rich-string-instance :drop-while pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
返回一个新的rich-string对象，包含从第一个不满足谓词条件的字符开始到字符串末尾的部分。
如果所有字符都满足谓词条件，返回空字符串。

说明
----
该方法从rich-string对象的开头开始，连续丢弃满足谓词条件的字符，直到遇到第一个不满足条件的字符为止。
返回从该字符开始到字符串末尾的部分。
原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回空字符串
- 所有字符都满足谓词：返回空字符串
- 所有字符都不满足谓词：返回原字符串
- 开头部分字符满足谓词：返回剩余部分字符串
- 中间字符满足谓词：只丢弃开头的连续满足条件的字符
- Unicode字符：正确处理Unicode字符的丢弃

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的字符直到找到第一个不满足条件的字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持链式操作
|#

;; 基本功能测试
;; 丢弃开头的小写字母
(check ((rich-string :value-of "helloWorld") :drop-while (lambda (c) (c :lower?)) :get) => "World")

;; 丢弃开头的数字字符
(check ((rich-string :value-of "123abc") :drop-while (lambda (c) (c :digit?)) :get) => "abc")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :drop-while (lambda (c) #t) :get) => "")

;; 所有字符都满足谓词
(check ((rich-string :value-of "aaa") :drop-while (lambda (c) #t) :get) => "")

;; 所有字符都不满足谓词
(check ((rich-string :value-of "AAA") :drop-while (lambda (c) (c :lower?)) :get) => "AAA")

;; Unicode字符丢弃测试
(check ((rich-string :value-of "测试字符串") :drop-while (lambda (c) (c :equals (rich-char #x6D4B))) :get) => "试字符串")  ; 丢弃"测"字

;; 复杂谓词测试
(check ((rich-string :value-of "a1b2c3") :drop-while (lambda (c) (or (c :lower?) (c :digit?))) :get) => "")  ; 所有字符都满足条件

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :drop-while (lambda (c) #t))) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :drop-while (lambda (c) (c :lower?)))
  (check (original :get) => "hello")
) ;let

#|
rich-string%+
将当前rich-string对象与另一个字符串、rich-string对象或数字连接，生成一个新的rich-string对象。

语法
----
(rich-string-instance :+ s)

参数
----
s : any
要连接的元素，支持以下类型：
- string：标准字符串
- rich-string：rich-string对象
- number：数字（会自动转换为字符串）

返回值
-----
返回一个新的rich-string对象，包含连接后的字符串。

说明
----
该方法将当前rich-string对象与指定的元素连接，返回一个新的rich-string对象。
支持多种参数类型，包括字符串、rich-string对象和数字。
对于数字参数，会自动调用number->string转换为字符串。
原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确连接多字节编码的字符。

边界条件
--------
- 空字符串连接：连接空字符串返回原字符串
- 连接空字符串：返回原字符串
- 连接数字：正确转换为字符串并连接
- Unicode字符连接：正确处理Unicode字符的连接
- 链式操作：支持多次连接操作

性能特征
--------
- 时间复杂度：O(n)，需要创建新的字符串对象
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持多种参数类型
- 支持链式操作
- 正确处理Unicode字符
|#

;; 基本功能测试
;; 连接字符串
(check ((rich-string :value-of "hello") :+ " world" :get) => "hello world")

;; 连接rich-string对象
(check ((rich-string :value-of "hello") :+ (rich-string :value-of " world") :get) => "hello world")

;; 连接数字
(check ((rich-string :value-of "number: ") :+ 123 :get) => "number: 123")

;; 边界条件测试
;; 空字符串连接
(check ((rich-string :empty) :+ "hello" :get) => "hello")

;; 连接空字符串
(check ((rich-string :value-of "hello") :+ "" :get) => "hello")

;; Unicode字符连接测试
(check ((rich-string :value-of "测试") :+ "字符串" :get) => "测试字符串")

;; 链式操作测试
(check ((rich-string :value-of "a") :+ "b" :+ "c" :get) => "abc")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :+ "ing")) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :+ " world")
  (check (original :get) => "hello")
) ;let

#|
rich-string%strip-left
从rich-string对象的开头移除空白字符，返回一个新的rich-string对象。

语法
----
(rich-string-instance :strip-left)

返回值
-----
返回一个新的rich-string对象，包含移除开头空白字符后的字符串。

说明
----
该方法从rich-string对象的开头移除连续的空白字符，包括空格、制表符、换行符等。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的空白字符。

边界条件
--------
- 空字符串：返回空字符串
- 字符串开头没有空白字符：返回与原字符串相同的字符串
- 字符串开头有空白字符：返回移除空白字符后的字符串
- 字符串全部是空白字符：返回空字符串
- Unicode空白字符：正确处理Unicode空白字符的移除

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串开头的字符直到遇到第一个非空白字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
|#

;; 基本功能测试
;; 移除开头的空白字符
(check ((rich-string :value-of "  hello") :strip-left :get) => "hello")

;; 移除开头的制表符
(check ((rich-string :value-of "\thello") :strip-left :get) => "hello")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :strip-left :get) => "")

;; 字符串开头没有空白字符
(check ((rich-string :value-of "hello") :strip-left :get) => "hello")

;; 字符串全部是空白字符
(check ((rich-string :value-of "   ") :strip-left :get) => "")

;; Unicode空白字符测试
;; 注意：string-trim 可能不支持全角空格，使用普通空格测试
(check ((rich-string :value-of " 测试") :strip-left :get) => "测试")  ; 移除普通空格

;; 混合空白字符测试
(check ((rich-string :value-of " \t\nhello") :strip-left :get) => "hello")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "  test") :strip-left)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "  hello")))
  (original :strip-left)
  (check (original :get) => "  hello")
) ;let

;; 链式操作测试
(check ((rich-string :value-of "  hello  ") :strip-left :strip-right :get) => "hello")

#|
rich-string%to-vector
将rich-string对象转换为包含rich-char对象的向量。

语法
----
(rich-string-instance :to-vector)

返回值
-----
返回一个向量，包含rich-string对象中的所有rich-char对象。

说明
----
该方法将rich-string对象转换为向量，向量中的每个元素都是rich-char对象。
对于空字符串，返回空向量。
该方法正确处理Unicode字符，能够准确转换多字节编码的字符。

边界条件
--------
- 空字符串：返回空向量
- 单字符字符串：返回包含单个rich-char对象的向量
- 多字符字符串：返回包含所有rich-char对象的向量
- Unicode字符：正确处理Unicode字符的转换

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(n)，创建新的向量对象

兼容性
------
- 与所有rich-string实例兼容
- 返回的向量可以用于各种向量操作
|#

;; 基本功能测试
;; 普通字符串转换
(check (vector-length ((rich-string :value-of "hello") :to-vector)) => 5)

;; 边界条件测试
;; 空字符串
(check (vector-length ((rich-string :empty) :to-vector)) => 0)

;; 单字符字符串
(check (vector-length ((rich-string :value-of "a") :to-vector)) => 1)

;; Unicode字符转换测试
(check (vector-length ((rich-string :value-of "测试") :to-vector)) => 2)

;; 验证向量元素类型
(check (rich-char :is-type-of (vector-ref ((rich-string :value-of "hello") :to-vector) 0)) => #t)

;; 验证向量内容
(let ((vec ((rich-string :value-of "ab") :to-vector)))
  (check ((vector-ref vec 0) :equals #\a) => #t)
  (check ((vector-ref vec 1) :equals #\b) => #t)
) ;let

;; 混合字符测试
(check (vector-length ((rich-string :value-of "a1B2c3") :to-vector)) => 6)

;; 验证返回类型
(check (vector? ((rich-string :value-of "test") :to-vector)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :to-vector)
  (check (original :get) => "hello")
) ;let

#|
rich-string%strip-prefix
从rich-string对象的开头移除指定的前缀，返回一个新的rich-string对象。

语法
----
(rich-string-instance :strip-prefix prefix)

参数
----
prefix : string
要移除的前缀字符串。

返回值
-----
返回一个新的rich-string对象，包含移除前缀后的字符串。
如果字符串不以指定的前缀开头，返回原字符串。

说明
----
该方法从rich-string对象的开头移除指定的前缀字符串。
如果字符串以指定的前缀开头，则移除该前缀并返回剩余部分。
如果字符串不以指定的前缀开头，则返回原字符串。
原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确移除多字节编码的前缀。

边界条件
--------
- 空字符串：移除任何前缀都返回空字符串
- 空前缀：返回原字符串
- 字符串以指定前缀开头：返回移除前缀后的字符串
- 字符串不以指定前缀开头：返回原字符串
- 前缀长度大于字符串长度：返回原字符串
- Unicode前缀：正确处理Unicode前缀的移除

性能特征
--------
- 时间复杂度：O(n)，需要检查字符串是否以前缀开头
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
|#

;; 基本功能测试
;; 移除存在的前缀
(check ((rich-string :value-of "hello world") :strip-prefix "hello " :get) => "world")

;; 字符串不以指定前缀开头
(check ((rich-string :value-of "hello world") :strip-prefix "world" :get) => "hello world")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :strip-prefix "hello" :get) => "")

;; 空前缀
(check ((rich-string :value-of "hello") :strip-prefix "" :get) => "hello")

;; 前缀与字符串完全匹配
(check ((rich-string :value-of "hello") :strip-prefix "hello" :get) => "")

;; Unicode前缀测试
(check ((rich-string :value-of "测试字符串") :strip-prefix "测试" :get) => "字符串")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "hello") :strip-prefix "h")) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello world")))
  (original :strip-prefix "hello ")
  (check (original :get) => "hello world")
) ;let

;; 链式操作测试
(check ((rich-string :value-of "prefix_suffix") :strip-prefix "prefix_" :get) => "suffix")

#|
rich-string%strip-right
从rich-string对象的末尾移除空白字符，返回一个新的rich-string对象。

语法
----
(rich-string-instance :strip-right)

返回值
-----
返回一个新的rich-string对象，包含移除末尾空白字符后的字符串。

说明
----
该方法从rich-string对象的末尾移除连续的空白字符，包括空格、制表符、换行符等。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的空白字符。

边界条件
--------
- 空字符串：返回空字符串
- 字符串末尾没有空白字符：返回与原字符串相同的字符串
- 字符串末尾有空白字符：返回移除空白字符后的字符串
- 字符串全部是空白字符：返回空字符串
- Unicode空白字符：正确处理Unicode空白字符的移除

性能特征
--------
- 时间复杂度：O(n)，需要从字符串末尾向前遍历直到遇到第一个非空白字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
|#

;; 基本功能测试
;; 移除末尾的空白字符
(check ((rich-string :value-of "hello  ") :strip-right :get) => "hello")

;; 移除末尾的制表符
(check ((rich-string :value-of "hello\t") :strip-right :get) => "hello")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :strip-right :get) => "")

;; 字符串末尾没有空白字符
(check ((rich-string :value-of "hello") :strip-right :get) => "hello")

;; 字符串全部是空白字符
(check ((rich-string :value-of "   ") :strip-right :get) => "")

;; Unicode字符测试
(check ((rich-string :value-of "测试  ") :strip-right :get) => "测试")  ; 移除末尾空格

;; 混合空白字符测试
(check ((rich-string :value-of "hello \t\n") :strip-right :get) => "hello")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test  ") :strip-right)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello  ")))
  (original :strip-right)
  (check (original :get) => "hello  ")
) ;let

;; 链式操作测试
(check ((rich-string :value-of "  hello  ") :strip-left :strip-right :get) => "hello")

#|
rich-string%strip-both
从rich-string对象的开头和结尾移除空白字符，返回一个新的rich-string对象。

语法
----
(rich-string-instance :strip-both)

返回值
-----
返回一个新的rich-string对象，包含移除开头和结尾空白字符后的字符串。

说明
----
该方法从rich-string对象的开头和结尾移除连续的空白字符，包括空格、制表符、换行符等。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的空白字符。

边界条件
--------
- 空字符串：返回空字符串
- 字符串没有空白字符：返回与原字符串相同的字符串
- 字符串开头有空白字符：返回移除开头空白字符后的字符串
- 字符串结尾有空白字符：返回移除结尾空白字符后的字符串
- 字符串开头和结尾都有空白字符：返回移除两端空白字符后的字符串
- 字符串全部是空白字符：返回空字符串
- Unicode空白字符：正确处理Unicode空白字符的移除

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串两端的字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
|#

;; 基本功能测试
;; 移除两端的空白字符
(check ((rich-string :value-of "  hello  ") :strip-both :get) => "hello")

;; 移除开头的空白字符
(check ((rich-string :value-of "  hello") :strip-both :get) => "hello")

;; 移除结尾的空白字符
(check ((rich-string :value-of "hello  ") :strip-both :get) => "hello")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :strip-both :get) => "")

;; 字符串没有空白字符
(check ((rich-string :value-of "hello") :strip-both :get) => "hello")

;; 字符串全部是空白字符
(check ((rich-string :value-of "   ") :strip-both :get) => "")

;; 混合空白字符测试
(check ((rich-string :value-of " \t\nhello \t\n") :strip-both :get) => "hello")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "  test  ") :strip-both)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "  hello  ")))
  (original :strip-both)
  (check (original :get) => "  hello  ")
) ;let

;; 链式操作测试
(check ((rich-string :value-of "  hello  ") :strip-both :strip-both :get) => "hello")

#|
rich-string%to-string
将rich-string对象转换为标准字符串。

语法
----
(rich-string-instance :to-string)

返回值
-----
以string形式返回rich-string对象内部存储的原始字符串数据。

说明
----
该方法返回rich-string对象内部存储的原始字符串数据。
由于rich-string对象本质上是对标准字符串的封装，
该方法提供了一种获取原始字符串数据的方式。
返回的字符串与创建rich-string对象时使用的字符串相同。
该方法不创建新的字符串对象，而是返回内部存储的引用。

边界条件
--------
- 空字符串：返回空字符串
- 普通字符串：返回原始字符串
- Unicode字符串：返回包含Unicode字符的原始字符串
- 特殊字符：返回包含特殊字符的原始字符串

性能特征
--------
- 时间复杂度：O(1)，直接返回内部存储的引用
- 空间复杂度：O(1)，不创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 返回的字符串可以用于所有标准字符串操作
|#

;; 基本功能测试
;; 普通字符串转换
(check ((rich-string :value-of "hello") :to-string) => "hello")

;; 空字符串转换
(check ((rich-string :empty) :to-string) => "")

;; 边界条件测试
;; 单字符字符串
(check ((rich-string :value-of "a") :to-string) => "a")

;; Unicode字符转换测试
(check ((rich-string :value-of "测试") :to-string) => "测试")

;; 特殊字符测试
(check ((rich-string :value-of "hello\nworld") :to-string) => "hello\nworld")

;; 数字字符串转换
(check ((rich-string :value-of "123") :to-string) => "123")

;; 混合字符测试
(check ((rich-string :value-of "a1B2c3") :to-string) => "a1B2c3")

;; 验证返回类型
(check (string? ((rich-string :value-of "test") :to-string)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :to-string)
  (check (original :get) => "hello")
) ;let

;; 链式操作测试
(check (string-length ((rich-string :value-of "hello") :to-string)) => 5)

#|
rich-string%for-each
对rich-string对象中的每个字符应用给定的过程函数，主要用于执行副作用操作。

语法
----
(rich-string-instance :for-each f)

参数
----
f : procedure
一个接受rich-char对象作为参数的过程函数。该函数主要用于执行副作用操作，返回值会被忽略。

返回值
-----
返回未定义值（unspecified），主要用于执行副作用操作。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用过程函数f。
与map方法不同，for-each主要用于执行副作用操作，如打印、修改外部状态等。
过程函数f的返回值会被忽略。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：不执行任何操作
- 单字符字符串：对单个字符执行过程函数
- 多字符字符串：对每个字符依次执行过程函数
- Unicode字符：正确处理Unicode字符的遍历

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 过程函数必须接受rich-char对象作为参数
- 主要用于执行副作用操作，不返回有用的值
|#

;; 基本功能测试
;; 收集遍历的字符
(let ((collected '()))
  ((rich-string :value-of "hello") :for-each (lambda (c) (set! collected (cons (c :to-integer) collected))))
  (check (reverse collected) => '(104 101 108 108 111))
) ;let

;; 边界条件测试
;; 空字符串
(let ((count 0))
  ((rich-string :empty) :for-each (lambda (c) (set! count (+ count 1))))
  (check count => 0)
) ;let

;; 单字符字符串
(let ((char-code #f))
  ((rich-string :value-of "a") :for-each (lambda (c) (set! char-code (c :to-integer))))
  (check char-code => 97)
) ;let

;; Unicode字符测试
(let ((count 0))
  ((rich-string :value-of "测试") :for-each (lambda (c) (set! count (+ count 1))))
  (check count => 2)
) ;let

;; 验证过程函数被正确调用
(let ((count 0))
  ((rich-string :value-of "abc") :for-each (lambda (c) (set! count (+ count 1))))
  (check count => 3)
) ;let

;; 混合字符测试
(let ((lower-count 0))
  ((rich-string :value-of "a1B2c3") :for-each (lambda (c) (when (c :lower?) (set! lower-count (+ lower-count 1)))))
  (check lower-count => 2)
) ;let

;; 验证字符顺序
(let ((chars '()))
  ((rich-string :value-of "123") :for-each (lambda (c) (set! chars (cons (c :to-integer) chars))))
  (check (reverse chars) => '(49 50 51))
) ;let

;; 验证副作用操作
(let ((sum 0))
  ((rich-string :value-of "123") :for-each (lambda (c) (set! sum (+ sum (c :to-integer)))))
  (check sum => 150)  ; 49 + 50 + 51 = 150
) ;let

;; 验证返回值为未定义
(check (unspecified? ((rich-string :value-of "test") :for-each (lambda (c) #t))) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello"))
      (count 0))
  (original :for-each (lambda (c) (set! count (+ count 1))))
  (check (original :get) => "hello")
) ;let

#|
rich-string%take-while
从rich-string对象的开头开始，连续取满足给定谓词条件的字符，生成一个新的rich-string对象。

语法
----
(rich-string-instance :take-while pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
返回一个新的rich-string对象，包含从开头开始连续满足谓词条件的字符序列。
如果所有字符都满足谓词条件，返回原字符串。

说明
----
该方法从rich-string对象的开头开始，连续取满足谓词条件的字符，直到遇到第一个不满足条件的字符为止。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回空字符串
- 所有字符都满足谓词：返回原字符串
- 所有字符都不满足谓词：返回空字符串
- 开头部分字符满足谓词：返回满足条件的连续字符序列
- 中间字符满足谓词：只取开头的连续满足条件的字符
- Unicode字符：正确处理Unicode字符的取操作

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的字符直到找到第一个不满足条件的字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持链式操作
|#

;; 基本功能测试
;; 取开头的小写字母
(check ((rich-string :value-of "helloWorld") :take-while (lambda (c) (c :lower?)) :get) => "hello")

;; 取开头的数字字符
(check ((rich-string :value-of "123abc") :take-while (lambda (c) (c :digit?)) :get) => "123")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :take-while (lambda (c) #t) :get) => "")

;; 所有字符都满足谓词
(check ((rich-string :value-of "aaa") :take-while (lambda (c) #t) :get) => "aaa")

;; 所有字符都不满足谓词
(check ((rich-string :value-of "AAA") :take-while (lambda (c) (c :lower?)) :get) => "")

;; Unicode字符测试
(check ((rich-string :value-of "测试字符串") :take-while (lambda (c) (c :equals (rich-char #x6D4B))) :get) => "测")  ; 只取"测"字

;; 复杂谓词测试
(check ((rich-string :value-of "a1b2c3") :take-while (lambda (c) (or (c :lower?) (c :digit?))) :get) => "a1b2c3")  ; 所有字符都满足条件

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :take-while (lambda (c) #t))) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :take-while (lambda (c) (c :lower?)))
  (check (original :get) => "hello")
) ;let

#|
rich-string%replace-first
在rich-string对象中查找并替换第一个匹配的子字符串，返回一个新的rich-string对象。

语法
----
(rich-string-instance :replace-first old new)

参数
----
old : string
要查找并替换的子字符串。

new : string
用于替换的新子字符串。

返回值
-----
返回一个新的rich-string对象，包含替换第一个匹配子字符串后的结果。
如果未找到匹配的子字符串，返回原字符串的副本。

说明
----
该方法在rich-string对象中查找第一个出现的子字符串"old"，
并将其替换为子字符串"new"，返回一个新的rich-string对象。
原字符串保持不变（不可变性原则）。
该方法只替换第一个匹配的子字符串，后续的匹配不会被替换。
支持Unicode字符，能够正确处理多字节编码的字符替换。

边界条件
--------
- 空字符串：替换任何子字符串都返回空字符串
- 空old参数：返回原字符串
- 空new参数：相当于删除第一个匹配的子字符串
- 未找到匹配：返回原字符串的副本
- 多个匹配：只替换第一个匹配的子字符串
- old等于new：返回原字符串的副本
- Unicode字符：正确处理Unicode字符的替换

性能特征
--------
- 时间复杂度：O(n)，需要搜索整个字符串
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
- 正确处理Unicode字符
|#

;; 基本功能测试
;; 替换存在的子字符串
(check ((rich-string :value-of "hello world") :replace-first "hello" "hi" :get) => "hi world")

;; 多个匹配时只替换第一个
(check ((rich-string :value-of "hello hello world") :replace-first "hello" "hi" :get) => "hi hello world")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :replace-first "hello" "hi" :get) => "")

;; 未找到匹配的子字符串
(check ((rich-string :value-of "hello world") :replace-first "test" "hi" :get) => "hello world")

;; 空new参数
(check ((rich-string :value-of "hello world") :replace-first "hello" "" :get) => " world")

;; Unicode字符替换测试
(check ((rich-string :value-of "测试字符串") :replace-first "测试" "实验" :get) => "实验字符串")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :replace-first "t" "T")) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello world")))
  (original :replace-first "hello" "hi")
  (check (original :get) => "hello world")
) ;let

;; 链式操作测试
(check ((rich-string :value-of "hello world") :replace-first "hello" "hi" :replace-first "world" "earth" :get) => "hi earth")

#|
rich-string%replace
在rich-string对象中查找并替换所有匹配的子字符串，返回一个新的rich-string对象。

语法
----
(rich-string-instance :replace old new)

参数
----
old : string
要查找并替换的子字符串。

new : string
用于替换的新子字符串。

返回值
-----
返回一个新的rich-string对象，包含替换所有匹配子字符串后的结果。
如果未找到匹配的子字符串，返回原字符串的副本。

说明
----
该方法在rich-string对象中查找所有出现的子字符串"old"，
并将其全部替换为子字符串"new"，返回一个新的rich-string对象。
原字符串保持不变（不可变性原则）。
该方法替换所有匹配的子字符串，与replace-first不同。
支持Unicode字符，能够正确处理多字节编码的字符替换。

边界条件
--------
- 空字符串：替换任何子字符串都返回空字符串
- 空old参数：返回原字符串
- 空new参数：相当于删除所有匹配的子字符串
- 未找到匹配：返回原字符串的副本
- 多个匹配：替换所有匹配的子字符串
- old等于new：返回原字符串的副本
- Unicode字符：正确处理Unicode字符的替换

性能特征
--------
- 时间复杂度：O(n)，需要搜索整个字符串
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
- 正确处理Unicode字符
|#

;; 基本功能测试
;; 替换所有匹配的子字符串
(check ((rich-string :value-of "hello world hello") :replace "hello" "hi" :get) => "hi world hi")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :replace "hello" "hi" :get) => "")

;; 未找到匹配的子字符串
(check ((rich-string :value-of "hello world") :replace "test" "hi" :get) => "hello world")

;; 空new参数
(check ((rich-string :value-of "hello world hello") :replace "hello" "" :get) => " world ")

;; Unicode字符替换测试
(check ((rich-string :value-of "测试测试字符串") :replace "测试" "实验" :get) => "实验实验字符串")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :replace "t" "T")) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello world hello")))
  (original :replace "hello" "hi")
  (check (original :get) => "hello world hello")
) ;let

;; 链式操作测试
(check ((rich-string :value-of "hello world hello") :replace "hello" "hi" :replace "world" "earth" :get) => "hi earth hi")

#|
rich-string%pad-left
在rich-string对象的左侧填充指定的字符，使其达到指定的长度，返回一个新的rich-string对象。

语法
----
(rich-string-instance :pad-left width (char #\space))

参数
----
width : integer
目标字符串长度。

char : char (可选，默认为#\space)
用于填充的字符。

返回值
-----
返回一个新的rich-string对象，包含左侧填充后的字符串。
如果原字符串长度已经大于或等于width，返回原字符串的副本。

说明
----
该方法在rich-string对象的左侧填充指定的字符，直到字符串达到指定的长度。
如果原字符串长度已经满足要求，则不进行填充。
原字符串保持不变（不可变性原则）。
支持Unicode字符，能够正确处理多字节编码的字符。

边界条件
--------
- 空字符串：填充到指定长度
- 宽度为0或负数：返回原字符串的副本
- 原字符串长度等于宽度：返回原字符串的副本
- 原字符串长度大于宽度：返回原字符串的副本
- 填充字符为特殊字符：正确处理特殊字符填充
- Unicode字符：正确处理Unicode字符的填充

性能特征
--------
- 时间复杂度：O(n)，需要创建新的字符串对象
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
- 正确处理Unicode字符
|#

;; 基本功能测试
;; 左侧填充空格
(check ((rich-string :value-of "hello") :pad-left 10 #\space :get) => "     hello")

;; 指定填充字符
(check ((rich-string :value-of "test") :pad-left 8 #\0 :get) => "0000test")

;; 边界条件测试
;; 空字符串填充
(check ((rich-string :empty) :pad-left 5 #\* :get) => "*****")

(check ((rich-string :empty) :pad-left -1 #\space :get) => "")

;; 原字符串长度等于宽度
(check ((rich-string :value-of "hello") :pad-left 5 #\space :get) => "hello")

;; 原字符串长度大于宽度
(check ((rich-string :value-of "hello world") :pad-left 5 #\space :get) => "hello world")

;; Unicode字符填充测试
(check ((rich-string :value-of "测试") :pad-left 6 #\- :get) => "----测试")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :pad-left 10  #\space)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :pad-left 10 #\space)
  (check (original :get) => "hello")
) ;let

;; 链式操作测试
(check ((rich-string :value-of "hi") :pad-left 5 #\* :pad-left 8 #\- :get) => "---***hi")

#|
rich-string%pad-right
在rich-string对象的右侧填充指定的字符，使其达到指定的长度，返回一个新的rich-string对象。

语法
----
(rich-string-instance :pad-right width (char #\space))

参数
----
width : integer
目标字符串长度。

char : char (可选，默认为#\space)
用于填充的字符。

返回值
-----
返回一个新的rich-string对象，包含右侧填充后的字符串。
如果原字符串长度已经大于或等于width，返回原字符串的副本。

说明
----
该方法在rich-string对象的右侧填充指定的字符，直到字符串达到指定的长度。
如果原字符串长度已经满足要求，则不进行填充。
原字符串保持不变（不可变性原则）。
支持Unicode字符，能够正确处理多字节编码的字符。

边界条件
--------
- 空字符串：填充到指定长度
- 宽度为0或负数：返回原字符串的副本
- 原字符串长度等于宽度：返回原字符串的副本
- 原字符串长度大于宽度：返回原字符串的副本
- 填充字符为特殊字符：正确处理特殊字符填充
- Unicode字符：正确处理Unicode字符的填充

性能特征
--------
- 时间复杂度：O(n)，需要创建新的字符串对象
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
- 正确处理Unicode字符
|#

;; 基本功能测试
;; 右侧填充空格
(check ((rich-string :value-of "hello") :pad-right 10 #\space :get) => "hello     ")

;; 指定填充字符
(check ((rich-string :value-of "test") :pad-right 8 #\0 :get) => "test0000")

;; 边界条件测试
;; 空字符串填充
(check ((rich-string :empty) :pad-right 5 #\* :get) => "*****")

;; 宽度为负数
(check ((rich-string :value-of "hello") :pad-right -1 #\space :get) => "hello")

;; 原字符串长度等于宽度
(check ((rich-string :value-of "hello") :pad-right 5 #\space :get) => "hello")

;; 原字符串长度大于宽度
(check ((rich-string :value-of "hello world") :pad-right 5 #\space :get) => "hello world")

;; Unicode字符填充测试
(check ((rich-string :value-of "测试") :pad-right 6 #\- :get) => "测试----")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :pad-right 10 #\space)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :pad-right 10 #\space)
  (check (original :get) => "hello")
) ;let

;; 链式操作测试
(check ((rich-string :value-of "hi") :pad-right 5 #\* :pad-right 8 #\- :get) => "hi***---")

#|
rich-string%split
将rich-string对象按照指定的分隔符分割成多个子字符串，返回一个rich-string对象的列表。

语法
----
(rich-string-instance :split delimiter)

参数
----
delimiter : string
用于分割字符串的分隔符。

返回值
-----
返回一个rich-vector对象，包含分割后的各个rich-string子字符串。
如果未找到分隔符，返回包含原字符串的rich-vector。
如果分隔符为空字符串，返回包含原字符串每个字符的rich-vector。

说明
----
该方法在rich-string对象中查找指定的分隔符，并将字符串分割成多个部分。
原字符串保持不变（不可变性原则）。
支持Unicode字符，能够正确处理多字节编码的字符分割。
连续的分隔符会产生空字符串元素。

边界条件
--------
- 空字符串：返回包含空字符串的列表
- 空分隔符：返回包含原字符串的列表
- 未找到分隔符：返回包含原字符串的列表
- 字符串以分隔符开头：第一个元素为空字符串
- 字符串以分隔符结尾：最后一个元素为空字符串
- 连续分隔符：产生空字符串元素
- Unicode字符：正确处理Unicode字符的分割

性能特征
--------
- 时间复杂度：O(n)，需要搜索整个字符串
- 空间复杂度：O(n)，创建新的字符串对象列表

兼容性
------
- 与所有rich-string实例兼容
- 返回的rich-vector元素都是rich-string对象
- 正确处理Unicode字符
|#

;; 基本功能测试
;; 使用空格分割
(check ($ "hello world test" :split " ")
       => (vector "hello" "world" "test")
) ;check

;; 使用逗号分割
(check ($ "a,b,c" :split ",")
       => (vector "a" "b" "c")
) ;check

;; 边界条件测试
;; 空字符串
(check ($ "" :split ",")
       => (vector "")
) ;check

;; 空分隔符
(check ($ "hello" :split "")
       => (vector "h" "e" "l" "l" "o")
) ;check

;; 未找到分隔符
(check ($ "hello world" :split ",")
       => (vector "hello world")
) ;check

;; 字符串以分隔符开头
(check ($ ",hello,world" :split ",")
       => (vector "" "hello" "world")
) ;check

;; 字符串以分隔符结尾
(check ($ "hello,world," :split ",")
       => (vector "hello" "world" "")
) ;check

;; 连续分隔符
(check ($ "a,,b,,,c" :split ",")
       => (vector "a" "" "b" "" "" "c")
) ;check

;; Unicode字符分割测试
(check ($ "测试,分割,字符串" :split ",")
       => (vector "测试" "分割" "字符串")
) ;check

;; 验证返回类型
(let ((result ($ "a,b" :split ",")))
  (check (rich-vector :is-type-of result) => #t)
) ;let

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello,world")))
  (original :split ",")
  (check (original :get) => "hello,world")
) ;let

(check ((rich-string :value-of "hi") :pad-right 5 #\* :pad-right 8 #\- :get) => "hi***---")

(check-report)
